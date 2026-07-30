;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.platform
  "JVM platform detection and FFI bootstrap. Consumer libraries share these
   functions.

   Library-specific filename and path knowledge stays at the consumer. This
   namespace owns only the library-agnostic shape:
     - The OS keyword
     - The architecture keyword
     - The dt-ffi backend selection
     - The resolution of a generated native fn
     - The extraction of a packaged library

   The extraction puts a native artifact on disk, where the loader can bind
   it."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [tech.v3.datatype.ffi :as dt-ffi])
  (:import [java.io File]
           [java.net JarURLConnection]
           [java.nio.file Files Path]))

(defn get-os
  "Return a keyword for the OS of the current JVM. The keyword is :darwin,
   :linux, :windows, or :android. Library-specific filename suffixes come
   from this keyword."
  []
  (let [vendor (string/lower-case (System/getProperty "java.vendor"))
        os     (string/lower-case (System/getProperty "os.name"))]
    (cond (string/includes? vendor "android") :android
          (string/includes? os "mac")         :darwin
          (string/includes? os "win")         :windows
          :else                               :linux)))

(defn get-arch
  "Return a keyword for the CPU architecture of the current JVM. The keyword
   is :amd64, :x86, :aarch64, or a normalized keyword for an unknown
   architecture."
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

(defn init-ffi!
  "Select the dt-ffi backend. The default is :jdk, which is Panama and
   java.lang.foreign. It is the only backend at this time, because JNA is
   gone.

   Each consumer wraps init-ffi! in its own init for one library. That init
   finds the native library, and binds symbols against fndefs."
  ([] (init-ffi! :jdk))
  ([backend] (dt-ffi/set-ffi-impl! backend)))

(defn resolve-native-fn
  "Resolve a generated dt-ffi native fn Var by fndef key `k` in `impl-ns`.
   `impl-ns` is a namespace symbol. Returns the Var, or nil.

   The dt-ffi define-library-functions macro interns these fns at load time.
   Thus resolve-native-fn must resolve them late, and no caller can capture
   them at compile time.

   The impl namespace must be loaded already. Its `require` at the call site
   is load-bearing, even when the alias looks unused."
  [impl-ns k]
  (ns-resolve impl-ns (symbol (name k))))

(defn make-native-fn-resolver
  "Return a resolver fn from fndef key to Var, over `impl-ns`. The options map
   has these keys:
     :throw?   Throw ex-info on a missing fn, and do not return nil.
     :memoize? Cache the resolved Vars for each key. This matches a memoized
               ffi-fn. Do not use it if a REPL can reload the impl namespace,
               because the cache holds the previous Vars.

   With no options, make-native-fn-resolver returns the equivalent of
   (partial resolve-native-fn impl-ns)."
  ([impl-ns] (make-native-fn-resolver impl-ns nil))
  ([impl-ns {:keys [throw? memoize?]}]
   (let [resolve1 (fn [k]
                    (or (resolve-native-fn impl-ns k)
                        (when throw?
                          (throw (ex-info (str "No native fn for " k)
                                          {:fn-key k :impl-ns impl-ns})))))]
     (if memoize? (memoize resolve1) resolve1))))

(defn apply-native-fn
  "Apply a dt-ffi var that is already resolved.

   A NULL const char* return makes the generated wrapper throw
   IllegalArgumentException, with PToPointer in the message. c->string has a
   zero-address branch that returns nil. But c->string calls ->pointer first,
   and PToPointer has no nil extension. Thus c->string never reaches that
   branch. NULL is a usual result for many C accessors, and it is not an
   error. apply-native-fn turns that one failure into nil.

   The upstream fix is a `when data` guard in c->string. An extension of
   PToPointer to nil would erase the dt-ffi :pointer and :pointer?
   distinction instead. The dt-ffi documentation says that :pointer throws on
   nil. Delete this catch when a dtype-next release carries the guard.
   Release 11.025 does not carry it."
  [f args]
  (try
    (apply f args)
    (catch IllegalArgumentException e
      (if (re-find #"PToPointer" (.getMessage e))
        nil
        (throw e)))))

(defn call-native-fn
  "Resolve the generated dt-ffi var for `fn-key` in `impl-ns`. Then apply it."
  [impl-ns fn-key args]
  (if-let [f (resolve-native-fn impl-ns fn-key)]
    (apply-native-fn f args)
    (throw (ex-info "Native function not found" {:fn fn-key :impl-ns impl-ns}))))

(defn init-jdk-library!
  "Load a native library on the :jdk (Panama) backend. First select the
   backend. Then bind `singleton` to the library at the canonical path of
   `file`. The bind happens only when `singleton` is not nil. The :jdk backend
   resolves by absolute path, with SymbolLookup.libraryLookup.

   A nil `singleton` means that the upstream extraction or probe failed. Then
   init-jdk-library! does no bind. It defers the failure to the first native
   call, which matches a guarded init."
  [singleton ^File file]
  (init-ffi! :jdk)
  (when singleton
    (dt-ffi/library-singleton-set! singleton (.getCanonicalPath file))))

(defn reset-library!
  "Reset a dt-ffi library singleton, so the next init binds it again. A nil
   singleton causes no change."
  [singleton]
  (when singleton
    (dt-ffi/library-singleton-reset! singleton)))

(defn libname-from-file
  "Derive a bare library name from a native library File. First strip a
   trailing version run after the extension. Then strip the extension. Then
   strip a LEADING `lib` only.

   Examples:
     - libproj.dylib -> proj
     - libz.so.1 -> z
     - glib-2.0.so -> glib-2.0
     - tifflib.dll -> tifflib

   tifflib.dll has no leading `lib`, thus libname-from-file strips nothing
   there. A version between the name and the extension is out of scope, as in
   libproj.25.dylib. No consumer passes that shape."
  [^File file]
  (-> (.getName file)
      (.replaceFirst "([.][0-9]+)+$" "")
      (.replaceFirst "[.][^.]+$" "")
      (.replaceFirst "^lib" "")))

(defn rehydrate-fn-defs
  "Return `fndefs` with every argument name changed from a keyword to a symbol.

   dt-ffi reads the first element of each argtype pair as a defn parameter
   name. It accepts a symbol only. A .cljc fndefs map holds those names as
   keywords, because squint can also compile that shape. Thus each consumer
   converts the map one time, on the JVM side, before it reaches dt-ffi."
  [fndefs]
  (into {}
        (map (fn [[fn-key fn-def]]
               [fn-key (update fn-def :argtypes
                               (fn [argtypes]
                                 (mapv (fn [[arg-name & more]]
                                         (into [(symbol (name arg-name))] more))
                                       argtypes)))])
             fndefs)))

(def default-library-suffixes
  "The file extension of a shared library, for each OS keyword. A consumer can
   have an artifact with a different extension on one OS. That consumer passes
   :suffixes to override the entry."
  {:darwin ".dylib" :linux ".so" :windows ".dll" :android ".so"})

(defn- copy-resource!
  "Copy classpath resource `resource-path` to `dest-file`. Then make the
   destination readable, writable and executable for its owner. An extracted
   library must be loadable whatever the umask is. Throw
   FileNotFoundException when the resource is not on the classpath.

   The permissions go on AFTER the copy. File.setReadable and its siblings
   fail and return false on a path that does not exist yet. Thus a permission
   change before the copy has no effect."
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
  "List every file below classpath directory `path`, as a path relative to
   that directory. resource-dir-files reads a directory on the filesystem, and
   also a directory inside a jar. Return nil when the directory is not on the
   classpath."
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
  "Return the final segment of a classpath path. A trailing slash has no
   effect."
  [path]
  (-> path (string/replace #"/$" "") (string/split #"/") last))

(defn- make-temp-dir
  "Create a temporary directory. Returns it as a File."
  ^File [prefix]
  (.toFile (Files/createTempDirectory
            prefix
            (into-array java.nio.file.attribute.FileAttribute []))))

(defn- extract-library-file!
  "Copy the packaged library for the current platform into `dir`. Returns the
   destination File. The resource path convention is
   `<os>-<arch>/<lib-basename><suffix>`."
  ^File [^File dir lib-basename suffixes]
  (let [os        (get-os)
        file-name (str lib-basename (get (merge default-library-suffixes suffixes) os))
        resource  (str (name os) "-" (name (get-arch)) "/" file-name)
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
    ;; Normalize to a trailing slash. resource-dir-files returns paths
    ;; relative to the directory, thus (str resource-dir rel) must have the
    ;; separator. Without it, "grids" and "a.tif" give the resource
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
  "Extract a packaged native library from classpath resources into a new
   temporary directory. Returns the state that a dt-ffi consumer must have to
   load the library.

   Options:
     :lib-basename     The library file name without the extension, such as
                       \"libproj\". The resource path is
                       `<os>-<arch>/<lib-basename><suffix>`.
     :fn-defs-var      The Var that holds the fndefs map, in the form that
                       rehydrate-fn-defs returns. The dt-ffi library singleton
                       reads the Var late. Thus this key takes a Var, and a
                       map does not work here.
     :tmp-prefix       The name prefix of the temporary directory. The default
                       is :lib-basename.
     :suffixes         Extension overrides for each OS.
                       extract-and-bind-library! merges them over
                       default-library-suffixes.
     :extra-resources  Sibling files that the library must have at run time.
                       Each entry is {:resource \"proj.db\"} for one file, or
                       {:resource-dir \"grids/\"} for a directory tree. Each
                       entry takes an optional :dest name inside the temporary
                       directory. These files land adjacent to the library,
                       thus the consumer reaches them through the returned
                       :path.

   Returns {:file :path :libname :singleton}, or {} when the extraction fails.
   A missing resource for the current platform is the usual cause. The empty
   map keeps the namespace loadable. It also defers the failure to the first
   native call, which is what a guarded init wants. extract-and-bind-library!
   logs the cause at warn level, because the deferred failure names the
   missing symbol and not the missing file.

   extract-and-bind-library! marks every extracted file delete-on-exit. It
   does not mark the temporary directory, because the JVM removes a directory
   only while that directory is empty."
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
  "Return the find-fn for define-library-fns!. That find-fn maps a fndef key
   to the bound native fn. It does the lookup through the library singleton in
   `state-atom`. `state-atom` holds the map that extract-and-bind-library!
   returned. The lookup is late, thus the library can load after these Vars
   exist."
  [state-atom]
  (fn [fn-key]
    (dt-ffi/library-singleton-find-fn (:singleton @state-atom) fn-key)))

(defmacro define-library-fns!
  "Intern one callable Var for each fndef entry in the CALLING namespace.

   `fn-defs-sym` names a Var in the calling namespace that holds the fndefs
   map. dt-ffi dereferences that Var while this macro expands, thus the Var
   must be defined already. `state-atom-sym` names the Var that holds the
   extract-and-bind-library! result.

   `check-error-sym` is optional. It names a fn or a macro of two arguments.
   Those arguments are the fn definition and the un-evaluated call.
   `check-error-sym` applies only to a fndef that sets :check-error? true. It
   must resolve in the calling namespace. Give no third argument when no fndef
   sets that flag.

   This wrapper stays a macro, because the Vars must land in the consumer
   namespace. A resolver over :ffi-impl-ns finds them there by fndef key."
  ([fn-defs-sym state-atom-sym]
   `(define-library-fns! ~fn-defs-sym ~state-atom-sym nil))
  ([fn-defs-sym state-atom-sym check-error-sym]
   `(dt-ffi/define-library-functions
      ~fn-defs-sym
      (library-fn-finder ~state-atom-sym)
      ~check-error-sym)))
