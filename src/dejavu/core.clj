(ns dejavu.core
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

(defn file-set-hash [file-set]
  (let [out-dir (fs/file ".work/.fileset_hash")
        _ (fs/create-dirs out-dir)
        out-file (fs/file out-dir "aggregate.txt")]
    (spit out-file "")
    (doseq [f file-set]
      (let [sf (sha1-file f)]
        (spit out-file (str sf ":" (sha1 (slurp f)) "\n") :append true)))
    (sha1 (slurp out-file))))

(defn gsutil [opts & args]
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
                     (str/includes? err "No URLs matched"))
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

(defn gs-assets [bucket f]
  (str bucket "/" (fs/file-name f)))

(defn resource [resource-dir f]
  (fs/file resource-dir (str/replace f #"^/" "")))

(defn find-dest [resource-dir f asset-vals]
  (some #(when (= (fs/file-name f) (fs/file-name %))
           (resource resource-dir %)) asset-vals))

(defn install-from-tmp-dir [resource-dir tmp-dir asset-vals]
  (doseq [f (fs/list-dir tmp-dir)]
    (fs/copy f (doto (find-dest resource-dir f asset-vals)
                 (-> fs/parent fs/create-dirs)))))

(defn download-assets [{:keys [resource-dir bucket assets-edn]}]
  (let [tmp-download-dir (str (fs/create-temp-dir))
        asset-map (-> (slurp assets-edn)
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
        (fs/copy assets-edn resource-dir {:replace-existing true})))))

#_(defn manifest [shas]
  {:asset-map (into {}
                    (map (fn [[file sha]]
                           (let [relative (fs/relativize "journal/server/resources/public" file)]
                             [(str (str "/" relative))
                              (str "/"(human-readable relative sha))]))
                         shas))})

#_(defn cached-browser-release [{:keys [fileset
                                      bucket
                                      build-fn
                                      output-dirs
                                      asset-glob
                                      resource-dir]}]
  (let [front-end-hash (fileset-hash fileset)
        remote-assets-edn (str bucket "/lookup/" front-end-hash)
        tmp-dir (fs/create-temp-dir)
        assets-edn (fs/file tmp-dir "asset_manifest.edn")
        res (cp remote-assets-edn assets-edn false)]
    (if (= ::not-found res)
      (do
        (run! fs/delete-tree output-dirs)
        (build-fn)
        (let [shas (sha512s asset-glob output-dirs)
              manifest (manifest shas)
              asset-vals (:asset-map manifest)]
          (let [tmp-upload-dir (str (fs/create-temp-dir))]
            (println "Preparing uploads dir" tmp-upload-dir)
            (doseq [[file sha] asset-vals]
              (prn :file (resource resource-dir file) :sha sha)
              (fs/copy (resource resource-dir file) (doto (fs/file tmp-upload-dir (subs sha 1))
                                                      (-> fs/parent (doto prn) fs/create-dirs))))
            (println "Uploading uploads dir")
            (cp (str tmp-upload-dir "/**") gs-assets-dir))
          (spit assets-edn (with-out-str ((requiring-resolve 'clojure.pprint/pprint)
                                          manifest)))
          (cp assets-edn remote-assets-edn)
          (fs/copy assets-edn resource-dir {:replace-existing true})
          (println (slurp assets-edn))
          (run! fs/delete-tree output-dirs)
          (download resource-dir assets-edn)))
      (download resource-dir assets-edn))))
