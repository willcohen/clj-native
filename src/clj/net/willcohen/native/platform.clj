;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.platform
  "JVM platform detection and dt-ffi bootstrap for consumer libraries: OS and
   arch keywords, backend selection, native fn lookup, and extraction of a
   packaged library to disk. File names and paths of a library stay with its
   consumer."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [tech.v3.datatype.ffi :as dt-ffi])
  (:import [java.io File]
           [java.net JarURLConnection]
           [java.nio.file Files Path]))

(defn get-os
  "Return :darwin, :linux, :windows or :android for this JVM."
  []
  (let [vendor (string/lower-case (System/getProperty "java.vendor"))
        os     (string/lower-case (System/getProperty "os.name"))]
    (cond (string/includes? vendor "android") :android
          (string/includes? os "mac")         :darwin
          (string/includes? os "win")         :windows
          :else                               :linux)))

(defn get-arch
  "Return :amd64, :x86 or :aarch64 for this JVM. Another os.arch becomes a
   keyword with `_` and `-` removed."
  []
  (let [arch (System/getProperty "os.arch")]
    (case arch
      "amd64"   :amd64
      "x86_64"  :amd64
      "x86-64"  :amd64
      "i386"    :x86
      "i486"    :x86
      "i586"    :x86
      "i686"    :x86
      "i786"    :x86
      "i886"    :x86
      "aarch64" :aarch64
      (keyword (string/replace arch #"[_-]" "")))))

(defn- musl-maps?
  "True when `maps`, the text of /proc/<pid>/maps, maps the musl loader."
  [maps]
  (boolean (re-find #"/ld-musl-[^/\s]+\.so" maps)))

(defn- process-maps
  "The text of /proc/self/maps. slurp fails on some kernels, where
   available() on a proc file gives EINVAL."
  ^String []
  (String. (Files/readAllBytes (.toPath (File. "/proc/self/maps")))))

(def ^:private musl-process?
  ;; A file test is not enough: a glibc distribution can install musl too.
  (delay (try (musl-maps? (process-maps))
              (catch Exception _ false))))

(defn- library-dirs
  "The resource dirs to try for a packaged library: <os>-<arch>, after
   <os>-<arch>-musl on musl Linux."
  ([] (library-dirs (get-os) (get-arch) (and (= :linux (get-os)) @musl-process?)))
  ([os arch musl?]
   (let [dir (str (name os) "-" (name arch))]
     (if musl? [(str dir "-musl") dir] [dir]))))

(defn init-ffi!
  "Select the dt-ffi backend. :jdk (Panama) is the default and the only
   backend."
  ([] (init-ffi! :jdk))
  ([backend] (dt-ffi/set-ffi-impl! backend)))

(defn resolve-native-fn
  "Return the dt-ffi Var for fndef key `k` in namespace symbol `impl-ns`,
   or nil. dt-ffi interns these Vars at load time, so require `impl-ns` at
   the call site even when its alias looks unused."
  [impl-ns k]
  (ns-resolve impl-ns (symbol (name k))))

(defn make-native-fn-resolver
  "Return a fn from fndef key to Var over `impl-ns`. Options:
     :throw?   Throw ex-info on a missing fn instead of returning nil.
     :memoize? Cache the Vars. Do not use it where a REPL can reload
               `impl-ns`, because the cache keeps the old Vars."
  ([impl-ns] (make-native-fn-resolver impl-ns nil))
  ([impl-ns {:keys [throw? memoize?]}]
   (let [resolve1 (fn [k]
                    (or (resolve-native-fn impl-ns k)
                        (when throw?
                          (throw (ex-info (str "No native fn for " k)
                                          {:fn-key k :impl-ns impl-ns})))))]
     (if memoize? (memoize resolve1) resolve1))))

(defn apply-native-fn
  "Apply resolved dt-ffi fn `f` to `args`. A NULL const char* result gives
   nil."
  [f args]
  (try
    (apply f args)
    ;; dt-ffi c->string calls ->pointer before its NULL check, so a NULL
    ;; result throws. Delete this catch when dt-ffi checks first.
    (catch IllegalArgumentException e
      (if (re-find #"PToPointer" (.getMessage e))
        nil
        (throw e)))))

(defn call-native-fn
  "Resolve and apply the dt-ffi var for `fn-key` in `impl-ns`. Throws
   ex-info when it is missing."
  [impl-ns fn-key args]
  (if-let [f (resolve-native-fn impl-ns fn-key)]
    (apply-native-fn f args)
    (throw (ex-info "Native function not found" {:fn fn-key :impl-ns impl-ns}))))

(defn init-jdk-library!
  "Select the :jdk backend and bind `singleton` to the canonical path of
   `file`, since SymbolLookup.libraryLookup needs an absolute path. A nil
   `singleton` (a failed extraction) skips the bind, and the failure comes
   at the first native call."
  [singleton ^File file]
  (init-ffi! :jdk)
  (when singleton
    (dt-ffi/library-singleton-set! singleton (.getCanonicalPath file))))

(defn reset-library!
  "Reset dt-ffi library `singleton`, so the next init binds it again. nil
   does nothing."
  [singleton]
  (when singleton
    (dt-ffi/library-singleton-reset! singleton)))

(defn libname-from-file
  "The bare library name of `file`: strip a trailing version run, then the
   extension, then a leading `lib`.

     libproj.dylib -> proj      libz.so.1 -> z
     glib-2.0.so -> glib-2.0    tifflib.dll -> tifflib

   A version before the extension (libproj.25.dylib) is not handled. No
   consumer passes one."
  [^File file]
  (-> (.getName file)
      (.replaceFirst "([.][0-9]+)+$" "")
      (.replaceFirst "[.][^.]+$" "")
      (.replaceFirst "^lib" "")))

(defn nullable-c-string
  "A C string of `s` for a :string? argument, or nil (NULL) for nil. Call
   it in a stack resource context, which frees the copy."
  [s]
  (some-> s dt-ffi/string->c))

(defn- dt-ffi-argtype
  [[arg-name t & more]]
  (into [(symbol (name arg-name)) (if (= :string? t) :pointer? t)] more))

(defn rehydrate-fn-defs
  "Return `fndefs` as dt-ffi reads them. Argument names become symbols,
   since a .cljc fndefs map holds keywords for squint. dt-ffi has no
   :string?: an argument becomes :pointer?, which dispatch/call! fills
   with nullable-c-string, and a return becomes :string."
  [fndefs]
  (update-vals fndefs
               (fn [fn-def]
                 (cond-> (update fn-def :argtypes #(mapv dt-ffi-argtype %))
                   (= :string? (:rettype fn-def)) (assoc :rettype :string)))))

(def default-library-suffixes
  "Shared library extension for each OS keyword. A consumer overrides an
   entry with :suffixes."
  {:darwin ".dylib" :linux ".so" :windows ".dll" :android ".so"})

(defn- copy-resource!
  "Copy classpath resource `resource-path` to `dest-file`, then make it rwx
   for its owner so it loads under any umask. Throws FileNotFoundException
   when the resource is missing. The permissions go on after the copy,
   because File.setReadable fails on a path that does not exist."
  [resource-path ^File dest-file]
  (if-let [url (io/resource resource-path)]
    (with-open [in  (.openStream url)
                out (java.io.FileOutputStream. dest-file)]
      (io/copy in out))
    (throw (java.io.FileNotFoundException.
            (str "Classpath resource not found: " resource-path))))
  (doto dest-file
    (.setReadable true true)
    (.setWritable true true)
    (.setExecutable true true)))

(defn- resource-dir-files
  "Paths of every file below classpath directory `path`, relative to it,
   from the filesystem or a jar. nil when `path` is not on the classpath."
  [path]
  (when-let [url (io/resource path)]
    (let [protocol (.getProtocol url)]
      (case protocol
        "file"
        (let [dir (io/file url)]
          (when (.isDirectory dir)
            (let [dir-path (.toPath dir)]
              (->> (file-seq dir)
                   (filter #(.isFile ^File %))
                   (map #(str (.relativize ^Path dir-path (.toPath ^File %))))))))

        "jar"
        (let [^JarURLConnection conn (.openConnection url)
              jar-file     (.getJarFile conn)
              entry-prefix (.getEntryName conn)]
          (->> (enumeration-seq (.entries jar-file))
               (map #(.getName %))
               (filter #(and (.startsWith ^String % entry-prefix)
                             (not (.endsWith ^String % "/"))))
               (map #(subs % (count entry-prefix)))))

        (throw (ex-info (str "Unsupported resource protocol: " protocol)
                        {:url url}))))))

(defn- last-path-segment
  "The last segment of a classpath path, with any trailing slash ignored."
  [path]
  (-> path (string/replace #"/$" "") (string/split #"/") last))

(defn- make-temp-dir
  "Create a temporary directory and return its File."
  ^File [prefix]
  (.toFile (Files/createTempDirectory
            prefix
            (into-array java.nio.file.attribute.FileAttribute []))))

(defn- extract-library-file!
  "Copy the packaged library, from the first of library-dirs that has it,
   into `dir`. Returns the destination File."
  ^File [^File dir lib-basename suffixes]
  (let [file-name (str lib-basename (get (merge default-library-suffixes suffixes) (get-os)))
        resources (map #(str % "/" file-name) (library-dirs))
        resource  (or (first (filter io/resource resources)) (first resources))
        dest      (File. dir file-name)]
    (doto dest .deleteOnExit)
    (copy-resource! resource dest)
    dest))

(defn- extract-extra-resource!
  "Copy one :extra-resources entry into `dir`."
  [^File dir {:keys [resource resource-dir dest] :as entry}]
  (cond
    resource
    (let [f (File. dir ^String (or dest (last-path-segment resource)))]
      (io/make-parents f)
      (doto f .deleteOnExit)
      (copy-resource! resource f))

    resource-dir
    ;; Force a trailing slash: "grids" and the relative "a.tif" would give
    ;; "gridsa.tif".
    (let [base (if (string/ends-with? resource-dir "/") resource-dir (str resource-dir "/"))
          sub  (File. dir ^String (or dest (last-path-segment resource-dir)))]
      (.mkdirs sub)
      (doseq [rel (resource-dir-files base)]
        (let [f (File. sub ^String rel)]
          (io/make-parents f)
          (doto f .deleteOnExit)
          (copy-resource! (str base rel) f))))

    :else
    (throw (ex-info "An :extra-resources entry needs :resource or :resource-dir"
                    {:entry entry}))))

(defn extract-and-bind-library!
  "Extract a packaged native library and its extra resources into a new
   temporary directory. Returns {:file :path :libname :singleton} for a
   dt-ffi consumer.

     :lib-basename     File name without extension, such as \"libproj\".
                       The resource is <os>-<arch>/<lib-basename><suffix>,
                       tried first under <os>-<arch>-musl/ on musl Linux.
     :fn-defs-var      Var of the fndefs map, as rehydrate-fn-defs returns
                       it. A Var, because the dt-ffi singleton reads it late.
     :tmp-prefix       Temporary directory prefix. Default :lib-basename.
     :suffixes         Extension overrides for each OS, merged over
                       default-library-suffixes.
     :extra-resources  Files the library needs at run time, copied next to
                       it under the returned :path. Each entry is
                       {:resource \"proj.db\"} or {:resource-dir \"grids/\"},
                       with an optional :dest name.

   On failure, usually a missing resource for this platform, logs a warning
   and returns {}. The namespace stays loadable, and the first native call
   fails. The warning names the file, which that failure does not.

   Every extracted file is delete-on-exit. The directory is not, because the
   JVM deletes only an empty directory."
  [{:keys [lib-basename fn-defs-var tmp-prefix suffixes extra-resources]}]
  (try
    (let [dir      (make-temp-dir (or tmp-prefix lib-basename))
          lib-file (extract-library-file! dir lib-basename suffixes)]
      (run! #(extract-extra-resource! dir %) extra-resources)
      {:file      lib-file
       :path      (.getCanonicalPath (.getParentFile lib-file))
       :libname   (libname-from-file lib-file)
       :singleton (dt-ffi/library-singleton fn-defs-var)})
    (catch Exception e
      (log/warn e (str "Could not extract packaged library " lib-basename
                       " for " (name (get-os)) "-" (name (get-arch))
                       "; deferring to the first native call"))
      {})))

(defn library-fn-finder
  "Return the find-fn for define-library-fns!. It looks up a fndef key in
   the singleton of `state-atom`, an extract-and-bind-library! result, at
   call time, so the library can load after the Vars exist."
  [state-atom]
  (fn [fn-key]
    (dt-ffi/library-singleton-find-fn (:singleton @state-atom) fn-key)))

(defmacro define-library-fns!
  "Intern one Var for each fndef in the calling namespace, where a resolver
   over :ffi-impl-ns finds it.

   `fn-defs-sym` names the Var there that holds the fndefs. dt-ffi derefs it
   during expansion, so it must exist already. `state-atom-sym` names the Var
   that holds the extract-and-bind-library! result. Optional
   `check-error-sym` names a fn or macro of the fn-def and the unevaluated
   call. It wraps each fndef with :check-error? true, and must resolve in
   the calling namespace."
  ([fn-defs-sym state-atom-sym]
   `(define-library-fns! ~fn-defs-sym ~state-atom-sym nil))
  ([fn-defs-sym state-atom-sym check-error-sym]
   `(dt-ffi/define-library-functions
      ~fn-defs-sym
      (library-fn-finder ~state-atom-sym)
      ~check-error-sym)))
