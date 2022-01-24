(ns nextjournal.dejavu
  (:require [alphabase.base58 :refer [encode] :rename {encode base-58}]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.tools.namespace.parse :as ns-parse])
  (:import [java.security MessageDigest]))

(defn sha [s algo]
  (let [instance (MessageDigest/getInstance algo)
        bytes (.digest instance (cond (string? s)
                                      (.getBytes s)
                                      (fs/exists? s)
                                      (fs/read-all-bytes s)
                                      :else (throw (IllegalArgumentException. (str (type s))))))
        string (base-58 bytes)]
    string))

(defn sha1 [s]
  (sha s "SHA-1"))

(defn sha512 [s]
  (sha s "SHA-512"))

(defn sha1-file [f]
  (let [fn (str/replace f fs/file-separator "|")
        fn (str fn ".sha1")]
    fn))

(defn file-set-hash
  "Returns combined sha1 of file-set contents."
  [file-set]
  (let [out-dir (fs/file ".work/.fileset_hash")
        _ (fs/create-dirs out-dir)
        out-file (fs/file out-dir "aggregate.txt")]
    (spit out-file "")
    (doseq [f (sort file-set)]
      (let [sf (sha1-file f)]
        (spit out-file (str sf ":" (sha1 (slurp f)) "\n") :append true)))
    (println "Aggregate sha-1 hash:")
    (println (slurp out-file))
    (println "SHA-1:" (sha1 (slurp out-file)))
    (sha1 (slurp out-file))))

(defn- gsutil [opts & args]
  (let [args (map str (into ["gsutil"] args))]
    (apply println args)
    (let [{:keys [out err]}
          (-> (p/process args (merge {:out :string
                                      :err :string}
                                     opts))
              p/check)]
      (when-not (str/blank? out)
        (print out))
      (when-not (str/blank? err)
        (print err))
      (flush))))

(defn gs-copy
  "Copies local or remote file to local or remote file using gs-util.
  When throw-when-missing is false and remote file wasn't found, returns :dejavu.core/not-found."
  ([from to]
   (gs-copy from to true))
  ([from to throw-when-missing?]
   (gs-copy from to throw-when-missing? nil))
  ([from to throw-when-missing? opts]
   (try (gsutil opts "cp" "-r" from to)
        ::success
        (catch Exception e
          (let [err (:err (ex-data e))]
            (if (and (not throw-when-missing?)
                     (and err (str/includes? err "No URLs matched")))
              ::not-found
              (throw e)))))))

(defn human-readable
  "Makes a human readable filename from given file and SHA.
  E.g. foo.html + sha -> foo-sha.html"
  [file sha]
  (let [[leading ext] (fs/split-ext file)]
    (str leading "-" sha "." ext)))

(defn- glob-sources [dir glob]
  (map str (fs/glob dir glob)))

(defn- find-clojure-file [cp-dirs ns-name]
  (let [f (str (str/replace ns-name "." "/") ".clj")]
    (some (fn [dir]
            (let [f (fs/file dir f)]
              (when (fs/exists? f)
                (str f)))) cp-dirs)))

(defn- find-clojure-files [cp-dirs ns-names]
  (keep #(find-clojure-file cp-dirs %) ns-names))

(defn cljs-files
  "Returns CLJS files + CLJ files that contain related macros from dirs."
  [dirs]
  (let [cp-dirs dirs
        direct-inputs (mapcat #(glob-sources % "**.{cljs,cljc}") cp-dirs)
        cljs-namespaces (map ns-file/read-file-ns-decl direct-inputs)
        cljs-deps (set (mapcat ns-parse/deps-from-ns-decl cljs-namespaces))
        clojure-files (find-clojure-files cp-dirs cljs-deps)
        ;; The assumption is that macro namespaces do not have dependencies on
        ;; other .clj namespaces. So far that assumption seems to hold.
        macro-files (keep #(when (str/includes? (slurp %) "defmacro")
                             %) clojure-files)]
    (println "Macro namespaces: "macro-files)
    (concat direct-inputs macro-files)))

(defn- gs-assets [bucket f]
  (str bucket "/" (fs/file-name f)))

(defn- resource [resource-dir f]
  (fs/file resource-dir (str/replace f #"^/" "")))

(defn- find-dest [resource-dir f asset-vals]
  (some #(when (= (fs/file-name f) (fs/file-name %))
           (resource resource-dir %)) asset-vals))

(defn- install-from-tmp-dir [resource-dir tmp-dir asset-vals]
  (doseq [f (fs/list-dir tmp-dir)]
    (fs/copy f (doto (find-dest resource-dir f asset-vals)
                 (-> fs/parent fs/create-dirs)))))

(defn download-assets
  "Downloads files in manifest's :asset-map from bucket to resource dir."
  [{:keys [resource-dir bucket manifest]}]
  (let [tmp-download-dir (str (fs/create-temp-dir))
        asset-map (-> (slurp manifest)
                      (edn/read-string)
                      :asset-map)
        asset-vals (vals asset-map)
        target-list (->> asset-vals
                         (map fs/file-name)
                         (map #(gs-assets bucket %)))
        target-input (str/join "\n" target-list)
        resources (map #(resource resource-dir %) asset-vals)]
    (if (every? fs/exists? resources)
      (println "Assets already downloaded!")
      (do
        (println "Downloading assets")
        (gs-copy "-I" tmp-download-dir true {:in target-input})
        (install-from-tmp-dir resource-dir tmp-download-dir asset-vals)
        (fs/copy manifest resource-dir {:replace-existing true})))))

(defn manifest
  "Produces manifest."
  [{:keys [resource-dir file-sha-map
           bucket]}]
  {:asset-map (into {}
                    (map (fn [[file sha]]
                           (let [relative (fs/relativize resource-dir file)]
                             [(str (str "/" relative))
                              (str bucket "/" (human-readable (if bucket
                                                                (fs/file-name relative)
                                                                relative) sha))]))
                         file-sha-map))})
