;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.build
  "Library-agnostic build primitives for clj-native consumers. Consumers
  invoke these from their own bb task bodies. Each consumer supplies the
  config for its own library, such as versions, flags and dep lists."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.tasks :as tasks]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn detect-host-platform
  "Returns {:os :darwin|:linux|:windows :arch :aarch64|:amd64}. This fn is
  public for consumer build scripts. It is bb-only, like everything in this
  namespace."
  []
  (let [os-name (System/getProperty "os.name")
        os-arch (System/getProperty "os.arch")
        os (cond
             (re-find (re-pattern "(?i)mac|darwin") os-name) :darwin
             (re-find (re-pattern "(?i)linux") os-name) :linux
             (re-find (re-pattern "(?i)windows") os-name) :windows
             :else :unknown)
        arch (cond
               (re-find (re-pattern "(?i)aarch64|arm64") os-arch) :aarch64
               (re-find (re-pattern "(?i)amd64|x86_64") os-arch) :amd64
               :else :unknown)]
    {:os os :arch arch}))

(defn safe-parallel-jobs
  "Calculate a safe -j value from the available memory. A heavy C++ build
  runs out of memory at high parallelism on a low-memory CI runner. Returns
  one job for each 3GB of available memory, with the CPU count as the
  maximum."
  []
  (let [{:keys [os]} (detect-host-platform)
        available-gb (try
                       (cond
                         (fs/exists? "/proc/meminfo")
                         (let [meminfo (slurp "/proc/meminfo")
                               kb (or (some->> meminfo
                                               (re-find (re-pattern "MemAvailable:\\s+(\\d+)"))
                                               second
                                               str/trim
                                               Long/parseLong)
                                      (some->> meminfo
                                               (re-find (re-pattern "MemFree:\\s+(\\d+)"))
                                               second
                                               str/trim
                                               Long/parseLong)
                                      4194304)]
                           (/ kb 1048576.0))

                         (= :darwin os)
                         (let [bytes (-> (tasks/shell {:out :string} "sysctl" "-n" "hw.memsize")
                                         :out str/trim Long/parseLong)]
                           (/ bytes 1073741824.0))

                         :else 8.0)
                       (catch Exception _ 8.0))
        cpus (.. Runtime getRuntime availableProcessors)]
    (min cpus (max 1 (int (/ available-gb 3))))))

(defn- file-sha256 [path]
  (let [digest (.digest (java.security.MessageDigest/getInstance "SHA-256")
                        (fs/read-all-bytes path))]
    (format "%064x" (BigInteger. 1 digest))))

(defn- download-whole!
  "Download url to dest-path through dest-path.part, so a failed download
  leaves no dest-path."
  [url dest-path]
  (let [part (str dest-path ".part")]
    (try
      (with-open [in (:body (http/get url {:as :stream}))]
        (io/copy in (fs/file part)))
      (fs/move part dest-path {:replace-existing true})
      (finally (fs/delete-if-exists part)))))

(defn download-archive
  "Download url to dest, unless dest exists. A failed download leaves no
  dest. With :sha256, check dest on each call, and delete it and throw on a
  mismatch."
  ([url dest] (download-archive url dest nil))
  ([url dest {:keys [sha256]}]
   (let [dest-path (str dest)]
     (if (fs/exists? dest-path)
       (println (fs/file-name dest-path) "already exists. Skipping.")
       (do
         (println "Downloading" (fs/file-name dest-path) "from" url "...")
         (fs/create-dirs (fs/parent dest-path))
         (download-whole! url dest-path)))
     (when sha256
       (let [actual (file-sha256 dest-path)]
         (when (not= (str/lower-case sha256) actual)
           (fs/delete dest-path)
           (throw (ex-info (str (fs/file-name dest-path) " has sha256 " actual
                                ", not " sha256 ". The file is deleted.")
                           {:dest dest-path :expected sha256 :actual actual}))))))))

(defn build-once!
  "Call build!, unless output exists and its stamp file holds the same
  args, the data that decides the build. Throws when args do not print as
  EDN, or when build! makes no output."
  [output args build!]
  (let [stamp (str output ".build-args.edn")
        edn   (pr-str args)]
    ;; A Path or a fn prints its identity hash, so its stamp never matches.
    (when (str/includes? edn "#object[")
      (throw (ex-info (str "The args of build-once! do not print as EDN: " edn) {:args edn})))
    (if (and (fs/exists? output)
             (fs/exists? stamp)
             (= edn (slurp stamp)))
      (println (fs/file-name output) "is up to date")
      (do (fs/delete-if-exists stamp)
          (fs/delete-if-exists output)
          (build!)
          (when-not (fs/exists? output)
            (throw (ex-info (str "The build made no " output) {:output (str output)})))
          (spit stamp edn)))))

(defn extract-archive
  "Extract the tar archive archive-file in base-dir, plain, gzip or bzip2,
  unless the dir extracted-dir at its top exists there. A failed extraction
  leaves no extracted-dir."
  [archive-file extracted-dir base-dir]
  (let [extracted-path (fs/path base-dir extracted-dir)]
    (if (fs/exists? extracted-path)
      (println "Directory" (str "'" extracted-dir "'") "already exists. Skipping extraction.")
      (let [tmp (fs/create-temp-dir {:dir base-dir :prefix (str extracted-dir ".part-")})]
        (println "Extracting" archive-file "to" (str extracted-path) "...")
        (try
          (tasks/shell {:dir (str tmp)} "tar" "xf" (str (fs/absolutize (fs/path base-dir archive-file))))
          (fs/move (fs/path tmp extracted-dir) extracted-path)
          (finally (fs/delete-tree tmp)))))))

(defn- cmd-prefix
  "Return the emscripten wrapper prefix for a build type, or an empty vector.
  :wasm uses the emconfigure, emmake and emcmake wrappers. :native runs the
  tools directly."
  [type tool]
  (case type
    :wasm (case tool
            :configure ["emconfigure"]
            :make      ["emmake" "make"]
            :cmake     ["emcmake" "cmake"])
    (case tool
      :configure []
      :make      ["make"]
      :cmake     ["cmake"])))

(defn- check-cflags!
  "Throw when CFLAGS or CXXFLAGS, from env else `inherited`, has no -O
  level. An explicit CFLAGS replaces the -O2 of configure, so the library
  would build at -O0."
  [env inherited]
  (doseq [k ["CFLAGS" "CXXFLAGS"]
          :let [v (get env k (get inherited k))]
          :when (and (some? v) (not (re-find #"(^|\s)-O([0-3sgz]|fast)?(\s|$)" v)))]
    (throw (ex-info (str k " \"" v "\" has no -O level. An explicit " k
                         " replaces the one of configure. Add -O2, or -O0 on purpose.")
                    {:env-var k :value v}))))

(defn build-autotools-library
  "Build a library with a configure, make and make install toolchain.

  Required keys:
    :type          :native or :wasm. :wasm wraps the tools with emconfigure
                   and emmake.
    :build-dir     The build directory. For an out-of-tree build, this is a
                   fresh directory below the source tree, and
                   build-autotools-library deletes and creates it again. For
                   an in-tree build (:in-tree? true), this is the source
                   directory itself.
    :install-dir   The --prefix target. build-autotools-library cleans it
                   before the install.
    :configure-args The arguments after ./configure, for example
                   [\"--disable-shared\"].

  Optional keys:
    :env           A map of more env vars, such as CC, CXX and CFLAGS.
    :configure-script The path to the configure script, relative to
                      :build-dir. The default is \"../configure\" for an
                      out-of-tree build. Override it to \"./configure\" for
                      an in-tree build such as zlib.
    :in-tree?      The build happens in :build-dir, which is the source
                   directory. build-autotools-library does no delete-tree of
                   build-dir. It runs 'make distclean' first when a Makefile
                   exists.
    :post-configure  A fn of no arguments. build-autotools-library calls it
                     after configure and before make. It is useful for a
                     library-specific Makefile patch, as with zlib on WASM.
    :parallel-jobs An int. The default comes from safe-parallel-jobs.
    :skip-if-exists The path to an artifact. When that path exists,
                    build-autotools-library skips the build.

  A library-specific autogen.sh, a source patch, or deep Makefile surgery
  belongs to the caller. Do that work before you invoke this fn, or supply
  :post-configure to run it between configure and make."
  [{:keys [type build-dir install-dir configure-args env
           configure-script in-tree? post-configure parallel-jobs skip-if-exists]
    :or {parallel-jobs (safe-parallel-jobs)
         env {}}}]
  (check-cflags! env (System/getenv))
  (when (or (nil? skip-if-exists)
            (not (fs/exists? skip-if-exists)))
    (let [configure-script (or configure-script
                               (if in-tree? "./configure" "../configure"))
          make-cmd (cmd-prefix type :make)]
      (fs/delete-tree install-dir)
      (if in-tree?
        (when (fs/exists? (fs/path build-dir "Makefile"))
          (try (apply tasks/shell {:dir (str build-dir)}
                      (concat make-cmd ["distclean"]))
               (catch Exception _ nil)))
        (do (fs/delete-tree build-dir)
            (fs/create-dirs build-dir)))
      (let [configure-cmd (concat (cmd-prefix type :configure)
                                  [configure-script
                                   (str "--prefix=" install-dir)]
                                  configure-args)]
        (apply tasks/shell {:dir (str build-dir) :extra-env env} configure-cmd))
      (when post-configure (post-configure))
      (apply tasks/shell {:dir (str build-dir) :extra-env env}
             (concat make-cmd ["-j" (str parallel-jobs)]))
      (apply tasks/shell {:dir (str build-dir) :extra-env env}
             (concat make-cmd ["install"])))))

(defn build-cmake-library
  "Build a library with CMake.

  Required keys:
    :type          :native or :wasm. :wasm wraps the tools with emcmake and
                   emmake.
    :src-dir       The directory that contains CMakeLists.txt.
    :build-dir     The out-of-tree build directory. build-cmake-library
                   cleans it and creates it again.
    :cmake-args    The CMake flags, including -DCMAKE_INSTALL_PREFIX=...
                   and so on.

  Optional keys:
    :env           A map of more env vars, such as CC, CXX and
                   CMAKE_VERBOSE_MAKEFILE.
    :install?      Run the install step after the build. The default is
                   true.
    :clean?        Delete :build-dir before the configure step. The default
                   is true. Pass false to keep the tree across invocations,
                   for an incremental rebuild of a heavy build. cmake reads
                   its cache again, and make rebuilds only what changed.
    :parallel-jobs An int. The default comes from safe-parallel-jobs.
    :skip-if-exists The path to an artifact. When that path exists,
                    build-cmake-library skips the build.
    :cache-file    The path to an initial-cache.cmake file. This adds the -C
                   flag.
    :pre-configure A fn of no arguments. build-cmake-library calls it after
                   it creates :build-dir, and before cmake runs. It is
                   useful for a write of initial-cache.cmake into
                   :build-dir, or for a library-specific CMakeLists patch.

  The consumer constructs :cmake-args, with every dep pointer such as
  -DSQLite3_INCLUDE_DIR and -DZLIB_LIBRARY. This fn does no library
  introspection."
  [{:keys [type src-dir build-dir cmake-args env install? clean? parallel-jobs
           skip-if-exists cache-file pre-configure]
    :or {install? true
         clean? true
         parallel-jobs (safe-parallel-jobs)
         env {}}}]
  (when (or (nil? skip-if-exists)
            (not (fs/exists? skip-if-exists)))
    (when clean? (fs/delete-tree build-dir))
    (fs/create-dirs build-dir)
    (when pre-configure (pre-configure))
    (let [cmake-cmd (cmd-prefix type :cmake)
          final-args (cond-> (vec cmake-args)
                       cache-file (as-> a (into ["-C" (str cache-file)] a))
                       true (conj (str src-dir)))
          build-cmd (if (= type :wasm)
                      (concat (cmd-prefix type :make) ["-j" (str parallel-jobs)])
                      ["cmake" "--build" "." "--parallel" (str parallel-jobs)])
          install-cmd (if (= type :wasm)
                        (concat (cmd-prefix type :make) ["install"])
                        ["cmake" "--install" "."])]
      (apply tasks/shell {:dir (str build-dir) :extra-env env}
             (concat cmake-cmd final-args))
      (apply tasks/shell {:dir (str build-dir) :extra-env env} build-cmd)
      (when install?
        (apply tasks/shell {:dir (str build-dir) :extra-env env} install-cmd)))))

(def ^:private emscripten-incoming-module-js-api-default
  "An exact copy of the emscripten default INCOMING_MODULE_JS_API, from its
  src/settings.js. A -sINCOMING_MODULE_JS_API flag replaces that default
  completely, and it does not extend the default. Thus this list must name
  every property that a consumer gives the module at run time.

  This list is identical to the emscripten default. Thus the node and browser
  module-property reads stay byte-for-byte unchanged. emcc-link appends
  `wasmBinary` only. Refer to :incoming-module-js-api. Sync this list with
  the emscripten settings.js again at each toolchain bump."
  ["ENVIRONMENT" "arguments" "canvas" "dynamicLibraries" "elementPointerLock"
   "instantiateWasm" "locateFile" "monitorRunDependencies" "noExitRuntime"
   "noInitialRun" "onAbort" "onExit" "onRuntimeInitialized" "postRun"
   "preInit" "preRun" "print" "printErr" "setStatus" "statusMessage"
   "stderr" "stdin" "stdout" "thisProgram" "wasm" "websocket"])

(defn- js-string-list
  "Render a seq of names as the bracketed, double-quoted list that the emcc
  -s flags expect, for example [\"ccall\",\"getValue\"]."
  [coll]
  (str "[" (str/join "," (map (fn [s] (str "\"" s "\"")) coll)) "]"))

(defn emcc-link
  "Link compiled objects into a WASM module with em++.

  Required keys:
    :build-dir        The working directory. emcc runs there, and it writes
                      the output there.
    :output-name      For example \"mylib.js\". emcc produces a .js file and
                      a .wasm file.
    :objects          A list of object files and static libs, in dependency
                      order.
    :exported-functions A list of C function names, each with a leading
                      underscore.

  Optional keys:
    :exported-runtime-methods For example [\"ccall\" \"cwrap\" \"getValue\"].
    :pthreads?        Adds -pthread, USE_PTHREADS and SHARED_MEMORY. The
                      default is false.
    :fetch?           Adds -sFETCH=1. The default is the value of
                      :pthreads?, which keeps the historical coupled
                      behavior. Pass false with :pthreads? true for threads
                      with no Fetch API. Pass true alone for FETCH with no
                      threads.
    :stack-size       Integer bytes for -sSTACK_SIZE. The default is
                      1048576. The inline note below says why the 64K
                      emscripten default is unsafe for a C++-heavy module.
    :pthread-pool-size Integer for -sPTHREAD_POOL_SIZE. The default is 1.
                      emcc-link emits it only with :pthreads? true. The
                      inline note below says what the one spare worker is
                      for.
    :pthread-pool-delay-load? Adds -sPTHREAD_POOL_DELAY_LOAD=1. The default
                      is true, and emcc-link emits it only with :pthreads?
                      true. At false, module init calls
                      addRunDependency('loading-workers'), which can outlast
                      the worker-router bootstrap timeout. Refer to the
                      inline note below.
    :environment      For example \"web,worker,node\". The default comes
                      from :pthreads?.
    :force-filesystem? Adds -sFORCE_FILESYSTEM=1. The default is false. Set
                      it when the C library reaches the filesystem only
                      through paths that the consumer stages at run time,
                      such as handler-fs stageFiles into MEMFS. emcc
                      otherwise drops the FS runtime, because no compiled
                      call site references it.
    :extra-flags      More emcc flags, for example [\"-I\" \"src/include\"].
                      These land before :objects.
    :link-flags       More emcc flags, placed immediately before :objects
                      and after :extra-flags. The default is []. The two
                      lists differ by position only. Use this one where the
                      order against the object list matters, as with a -L
                      search path or a --whole-archive pair.
    :module-name      The EXPORT_NAME value. The default is \"Module\".
    :allow-memory-growth? The default is true.
    :allow-table-growth?  The default is true.
    :maximum-memory   Integer bytes for -sMAXIMUM_MEMORY. The default is
                      2147483648 (2 GiB) when :pthreads? is true, and nil
                      otherwise. Pthreads with SHARED_MEMORY otherwise
                      clamps the WebAssembly.Memory maximum to the initial
                      value. That blocks growth at run time, even with
                      ALLOW_MEMORY_GROWTH=1.
    :modularize?      The default is true, and it produces MODULARIZE=1 with
                      EXPORT_ES6=1.
    :optimization     For example \"-O2\", which is the default.
    :incoming-module-js-api A list of the Module.* properties that the
                      module can read at run time. emcc-link emits it as
                      -sINCOMING_MODULE_JS_API, which replaces the
                      emscripten default list completely.

                      The default is the emscripten default with
                      \"wasmBinary\" added. Thus an embedding host, such as
                      a GraalVM polyglot loader, can give the module its
                      wasm bytes through Module.wasmBinary. Without
                      \"wasmBinary\", emscripten aborts at init with
                      \"`Module.wasmBinary` was supplied but `wasmBinary`
                      not included in INCOMING_MODULE_JS_API\". A node or
                      browser build fetches the .wasm itself, thus this
                      does not affect it. Pass [] to suppress the flag and
                      use the untouched emscripten default.

  emcc writes the output .js and .wasm files in :build-dir. That is the whole
  result. The return value is the babashka.process result of the em++ run,
  and no caller should read it. tasks/shell throws on a non-zero exit here,
  so a return carries no exit status to check.

  Note: the library-specific parts come from the consumer fndefs and the
  build output. Those parts are :exported-functions and the order of
  :objects. This fn does not know what is inside the WASM module."
  [{:keys [build-dir output-name objects exported-functions
           exported-runtime-methods pthreads? environment extra-flags
           module-name allow-memory-growth? allow-table-growth?
           modularize? optimization maximum-memory stack-size
           force-filesystem? link-flags pthread-pool-size pthread-pool-delay-load?
           incoming-module-js-api]
    :or {exported-runtime-methods []
         pthreads? false
         extra-flags []
         module-name "Module"
         allow-memory-growth? true
         allow-table-growth? true
         modularize? true
         optimization "-O2"
         stack-size 1048576
         force-filesystem? false
         link-flags []
         pthread-pool-size 1
         pthread-pool-delay-load? true
         incoming-module-js-api (conj emscripten-incoming-module-js-api-default
                                      "wasmBinary")}
    :as opts}]
  ;; `worker` is necessary even for a single-threaded build. worker-router
  ;; runs every handler module inside a Web Worker. Thus the emscripten
  ;; module must permit the worker environment. Without it, the module aborts
  ;; on pool spawn with "worker environment detected but not enabled at build
  ;; time".
  (let [environment (or environment (if pthreads? "web,worker,node" "web,worker,node,shell"))
        fetch? (if (contains? opts :fetch?) (boolean (:fetch? opts)) pthreads?)
        maximum-memory (if (some? maximum-memory)
                         maximum-memory
                         (when pthreads? 2147483648))
        exports-str (js-string-list exported-functions)
        runtime-str (js-string-list exported-runtime-methods)
        cmd (vec (concat ["em++" "-o" output-name optimization
                          "-fexceptions" "-fvisibility=default"
                          "--target=wasm32-unknown-emscripten" "--no-entry"]
                         (when pthreads? ["-pthread"])
                         extra-flags
                         (when force-filesystem? ["-s" "FORCE_FILESYSTEM=1"])
                         link-flags
                         (map str objects)
                         [(str "-sEXPORTED_FUNCTIONS=" exports-str)]
                         (when (seq exported-runtime-methods)
                           [(str "-sEXPORTED_RUNTIME_METHODS=" runtime-str)])
                         (when (seq incoming-module-js-api)
                           [(str "-sINCOMING_MODULE_JS_API="
                                 (js-string-list incoming-module-js-api))])
                         ["-s" (str "ENVIRONMENT=" environment)]
                         (when allow-memory-growth? ["-s" "ALLOW_MEMORY_GROWTH=1"])
                         ;; GROWABLE_ARRAYBUFFERS=0 keeps the wasm memory on a
                         ;; plain ArrayBuffer. At 1, memory.buffer is a RESIZABLE
                         ;; ArrayBuffer. Chrome and Firefox each reject
                         ;; TextDecoder.decode on a view over a resizable buffer.
                         ;; UTF8ArrayToString decodes exactly such a view. Thus
                         ;; every string-returning C function throws in a browser.
                         ;; Node accepts it, thus a node-only suite stays green and
                         ;; hides this.
                         ;;
                         ;; emscripten 6.0.2 is the one release that carries the
                         ;; hazard. It defaulted the setting to 1, and its
                         ;; UTF8ToString did not copy. 6.0.3 reverted the default
                         ;; to 0 and made the decode copy from a resizable buffer.
                         ;; Thus the flag is a no-op from 6.0.3 on.
                         ;;
                         ;; It stays because this layer cannot assert that floor.
                         ;; A consumer supplies its own emscripten through the
                         ;; mkCrossShells :extraDevInputs, and its
                         ;; clj-native.inputs.nixpkgs.follows discards the pin in
                         ;; this repo's flake.lock.
                         (when allow-memory-growth? ["-s" "GROWABLE_ARRAYBUFFERS=0"])
                         (when allow-table-growth? ["-s" "ALLOW_TABLE_GROWTH=1"])
                         (when maximum-memory ["-s" (str "MAXIMUM_MEMORY=" maximum-memory)])
                         (when modularize?
                           ["-s" "MODULARIZE=1"
                            "-s" "EXPORT_ES6=1"
                            "-s" (str "EXPORT_NAME=\"" module-name "\"")])
                         ;; STACK_SIZE. The 64K emscripten default is too small
                         ;; for a C++-heavy codepath, such as a templated
                         ;; transform or deep recursion. A stack overflow corrupts
                         ;; the dlmalloc state silently. It can then surface later
                         ;; as a mutex-deadlock abort or a heap-corrupt abort. 1 MB
                         ;; is a defensible default for any C++ wasm. The stack of
                         ;; each pthread defaults to STACK_SIZE when
                         ;; DEFAULT_PTHREAD_STACK_SIZE is 0, which is also the
                         ;; default.
                         ["-s" (str "STACK_SIZE=" stack-size)]
                         (when fetch? ["-s" "FETCH=1"])
                         ;; PTHREAD_POOL_SIZE=1 with PTHREAD_POOL_DELAY_LOAD=1. One
                         ;; worker spawns in advance, and it handles the rare
                         ;; wasm-side pthread_create. One example is a libcxx
                         ;; <thread> with a transitive reference from a cache.
                         ;; DELAY_LOAD=1 spawns that worker lazily. Thus module init
                         ;; does NOT call `addRunDependency('loading-workers')`, and
                         ;; the worker-router bootstrap stays below its 30s timeout.
                         ;; A consumer must still disable the transitive
                         ;; pthread_create at the source, for example with
                         ;; -DSQLITE_MAX_WORKER_THREADS=0, to keep that path cold.
                         (when pthreads?
                           (concat
                            ["-s" "USE_PTHREADS=1"
                             "-s" "SHARED_MEMORY=1"
                             "-s" (str "PTHREAD_POOL_SIZE=" pthread-pool-size)]
                            (when pthread-pool-delay-load?
                              ["-s" "PTHREAD_POOL_DELAY_LOAD=1"])))))]
    (apply tasks/shell {:dir (str build-dir)} cmd)))

(defn emcc-compile
  "Compile a single C or C++ source to an object file with `emcc -c`. This
  builds a module-local translation unit, such as a host-callback stub. That
  unit then links ahead of the library archives in the emcc-link :objects.

  Required keys:
    :source   The path to the .c or .cpp source.
    :output   The path to the .o output.

  Optional keys:
    :include-dirs  A seq of directories. Each one becomes -I<dir>. One
                   example is the directory of a generated cpl_config.h.
    :optimization  The default is \"-O3\".
    :pthreads?     Adds -pthread. Match the threading model of the link.
    :extra-flags   More emcc flags.

  emcc writes the object file at :output. As with emcc-link, the return value
  is the babashka.process result and no caller should read it."
  [{:keys [source output include-dirs optimization pthreads? extra-flags]
    :or {include-dirs [] optimization "-O3" pthreads? false extra-flags []}}]
  (let [cmd (vec (concat ["emcc" "-c" (str source) "-o" (str output)
                          optimization "-fexceptions"]
                         (when pthreads? ["-pthread"])
                         (mapcat (fn [d] ["-I" (str d)]) include-dirs)
                         extra-flags))]
    (apply tasks/shell cmd)))

(defn- extract-resource!
  "Copy a classpath resource to dest, and create the parent directories.
  Returns dest as a string. Returns nil when the resource is not on the
  classpath. The behavior is the same for a clj-native source checkout and
  for a jar."
  [resource-path dest]
  (when-let [resource (io/resource resource-path)]
    (fs/create-dirs (fs/parent dest))
    (with-open [in (io/input-stream resource)]
      (io/copy in (fs/file (str dest))))
    (str dest)))

(defn- resolve-containerfile
  "Return the path to a Containerfile. A local Containerfile in the current
  working directory comes first, as a consumer override. Without one, this fn
  extracts the shipped Containerfile from the clj-native classpath to a temp
  file, and returns that path."
  []
  (let [cwd (System/getProperty "user.dir")
        local (fs/path cwd "Containerfile")]
    (if (fs/exists? local)
      (str local)
      (let [tmp (fs/create-temp-file {:prefix "clj-native-Containerfile-"})]
        (or (extract-resource! "net/willcohen/native/Containerfile" tmp)
            (throw (ex-info "Containerfile not found on classpath or in cwd"
                            {:cwd cwd})))))))

(defn- clj-native-root
  "Find the clj-native repo root on the host. Returns nil when there is no
  checkout.

  Only a :local/root consumer gets a path. The classpath URL is then a plain
  file:, and the repo root sits five parents above the Containerfile
  resource. A published jar gives jar:file:, and this fn returns nil. Then
  vendor-clj-native extracts the flake from the jar instead."
  []
  (when-let [resource (io/resource "net/willcohen/native/Containerfile")]
    (let [url-str (str resource)]
      (when (str/starts-with? url-str "file:")
        ;; file:/.../clj-native/resources/net/willcohen/native/Containerfile
        ;; -> clj-native
        (let [p (-> resource .toURI fs/path)]
          (-> p fs/parent fs/parent fs/parent fs/parent fs/parent str))))))

(defn- vendor-from-checkout
  "Copy a clj-native checkout into dest. This serves the two uses of the
  vendor directory."
  [src dest]
  (doseq [entry ["flake.nix" "flake.lock" "deps.edn" "bb.edn"
                 "src" "resources"]]
    (let [from (fs/path src entry)
          to (fs/path dest entry)]
      (when (fs/exists? from)
        (if (fs/directory? from)
          (fs/copy-tree from to {:replace-existing true})
          (fs/copy from to {:replace-existing true})))))
  (str dest))

(defn- vendor-from-jar
  "Extract the flake only, from the jar into dest. Returns dest. Returns nil
  when either file is missing, and then this fn removes dest. That prevents a
  half-populated flake, which would fail in nix later."
  [dest]
  (let [extracted (doall (for [f ["flake.nix" "flake.lock"]]
                           (extract-resource! (str "net/willcohen/native/" f)
                                              (fs/path dest f))))]
    (if (every? some? extracted)
      (str dest)
      (do (fs/delete-tree dest) nil))))

(defn- vendor-clj-native
  "Materialize clj-native at <cwd>/<vendor-name>/ for a container build.
  Returns the absolute path. Returns nil when neither source is available.

  Two things consume the vendor directory:
    * --override-input clj-native path:./<vendor-name>, for the flake
    * a /clj-native symlink -> /build/<vendor-name>, for the bb classpath.
      This serves a consumer with :local/root \"../clj-native\" in its bb.edn.

  A checkout serves the two. A published jar gives the flake only, and
  --override-input needs nothing more. The flake outputs are pure nix, and
  they never reference self or the source tree. Such a consumer resolves the
  Clojure side from Maven, thus it has no use for the symlink. The vendor step
  out of the jar keeps the flake at the same version as the code, and it needs
  no network."
  [vendor-name]
  (let [dest (fs/path (System/getProperty "user.dir") vendor-name)]
    (fs/delete-tree dest)
    (fs/create-dirs dest)
    (if-let [src (clj-native-root)]
      (vendor-from-checkout src dest)
      (vendor-from-jar dest))))

(defn- platform-tag
  "Convert a platform such as 'linux/amd64' to a filesystem-safe tag such as
  'linux-amd64'."
  [platform]
  (str/replace platform "/" "-"))

(defn- expand-path
  "Replace $PLATFORM_TAG in a path string with the given tag."
  [path tag]
  (str/replace (str path) "$PLATFORM_TAG" tag))

(defn cross-compile-in-container
  "Cross-compile artifacts with podman or docker. This fn uses the clj-native
  Containerfile, or the local Containerfile of the consumer when one is
  present.

  Config keys:
    :platforms      A list of platform strings, for example
                    [\"linux/amd64\" \"linux/aarch64\" \"windows/amd64\"].
    :image-tag      The base image tag. This fn appends the platform tag to
                    it.
    :target         The Containerfile target stage. The default is
                    \"native-build\".
    :extract-paths  A list of container paths to copy out. Each one is a
                    string path, or a map {:from <container-path>}. This fn
                    expands $PLATFORM_TAG. Every path lands in
                    artifacts-<platform-tag>/ on the host.
                    Example: [\"/build/resources/$PLATFORM_TAG/.\"
                              \"/build/resources/mylib.dat\"
                              \"/build/resources/mylib.ini\"]
    :build-args     A map of more --build-arg values.
    :mounts         A list of {:host-path :container-path :read-only?} for
                    the --volume mounts, such as a local source tree.
    :on-artifacts   (fn [platform-tag artifacts-dir]). This fn calls it after
                    the extraction. The consumer does its project-specific
                    routing there, such as a DLL rename or a move of
                    resources. This fn cleans up the artifacts-dir after
                    :on-artifacts returns.

  Returns nil. Throws when it finds neither podman nor docker."
  [{:keys [platforms image-tag target extract-paths build-args mounts
           on-artifacts vendor-clj-native? vendor-name]
    :or {target "native-build"
         build-args {}
         mounts []
         on-artifacts (fn [_ _] nil)
         vendor-clj-native? true
         vendor-name "clj-native-vendor"}}]
  (let [container-cmd (or (fs/which "podman") (fs/which "docker"))
        _ (when-not container-cmd
            (throw (ex-info "Neither podman nor docker found on PATH" {})))
        containerfile (resolve-containerfile)
        current-arch (str/trim (:out (tasks/shell {:out :string} "uname" "-m")))
        vendor-dir (when vendor-clj-native?
                     (vendor-clj-native vendor-name))
        extra-nix-flags (when vendor-dir
                          (str "--override-input clj-native path:./" vendor-name))
        effective-build-args (cond-> build-args
                               extra-nix-flags
                               (assoc "EXTRA_NIX_FLAGS" extra-nix-flags))]
    (println "--> Using" (str container-cmd) "for cross-platform builds.")
    (println "    Using Containerfile:" containerfile)
    (when vendor-dir
      (println "    Vendored clj-native into" vendor-dir))
    (try
      (doseq [platform platforms]
        (let [tag (platform-tag platform)
              image (str image-tag ":" tag)
              artifacts-dir (str "artifacts-" tag)
              is-cross-compile? (not (or (and (= platform "linux/amd64")
                                              (contains? #{"x86_64" "amd64"} current-arch))
                                         (and (= platform "linux/aarch64")
                                              (contains? #{"aarch64" "arm64"} current-arch))))]
          (println "--> Starting cross-platform build for" platform)
          (when is-cross-compile?
            (println "    (Cross-compiling from" current-arch "to" platform ")"))
          (let [host-linux-platform (str "linux/" (case current-arch
                                                    ("x86_64" "amd64") "amd64"
                                                    ("aarch64" "arm64") "aarch64"
                                                    "amd64"))
                container-platform (if (or (str/starts-with? platform "windows/")
                                           is-cross-compile?)
                                     host-linux-platform
                                     platform)
                base-cmd [(str container-cmd) "build"
                          "-f" containerfile
                          "--platform" container-platform
                          "--target" target
                          "-t" image
                          "--build-arg" (str "TARGET_PLATFORM=" platform)]
                user-args (mapcat (fn [[k v]] ["--build-arg" (str k "=" v)])
                                  effective-build-args)
                mount-args (mapcat (fn [{:keys [host-path container-path read-only?]}]
                                     ["--volume"
                                      (str host-path ":" container-path
                                           (when read-only? ":ro"))])
                                   mounts)
                final-args (concat base-cmd user-args mount-args ["."])]
            (apply tasks/shell final-args))
          (println "Extracting artifacts from container...")
          (fs/delete-tree artifacts-dir)
          (fs/create-dirs artifacts-dir)
          (let [container-id (str/trim (:out (tasks/shell {:out :string}
                                                          container-cmd "create" image)))]
            (try
              (doseq [path-or-map extract-paths]
                (let [from (expand-path (if (map? path-or-map) (:from path-or-map) path-or-map)
                                        tag)]
                  (tasks/shell {:continue true} container-cmd "cp"
                               (str container-id ":" from)
                               artifacts-dir)))
              (on-artifacts tag (fs/file artifacts-dir))
              (finally
                (tasks/shell container-cmd "rm" container-id)
                (fs/delete-tree artifacts-dir))))
          (println "--> Completed build for" platform)))
      (finally
        (when vendor-dir
          (fs/delete-tree vendor-dir))))))

(defn- leading-comment-block
  "Return the unbroken run of `;;` lines at the top of `source`, rendered as
  `//` lines. Returns nil when the file opens with anything else.

  squint drops every Clojure comment, thus a copyright and SPDX header in a
  .cljc never reaches its .mjs. carry-header! puts it back."
  [source]
  (let [lines (take-while (fn [l] (str/starts-with? l ";;"))
                          (str/split-lines source))]
    (when (seq lines)
      (str (str/join "\n" (map (fn [l] (str "//" (subs l 2))) lines))
           "\n\n"))))

(defn- carry-header!
  "Prepend the leading comment block of `src-file` to `out-file`. Does
  nothing when the source has no such block, or when the output carries it
  already. This fn is idempotent, thus a rebuild does not stack headers."
  [src-file out-file]
  (when-let [header (leading-comment-block (slurp (fs/file src-file)))]
    (let [body (slurp (fs/file out-file))]
      (when-not (str/starts-with? body header)
        (spit (fs/file out-file) (str header body))))))

(defn squint-compile!
  "Compile a single .cljc to a .mjs adjacent to it. The compile runs in
  `dir`. This fn prints the squint stdout and stderr, throws on a non-zero
  exit, and prints a ready line. The squint:* and squint:test:* bb tasks
  share it, here and in each consumer.

  squint emits no Clojure comment, thus the copyright and SPDX header of the
  .cljc would not reach the .mjs that npm ships. squint-compile! copies the
  leading `;;` block of the source onto the output as `//` lines. The header
  of each tree therefore travels with its own artifacts.

  opts (optional map):
    :binary  The path to a squint executable. A relative path resolves
             against `dir`. The default is `npx squint`, which resolves
             node_modules/.bin/squint below `dir`. A task can compile in a
             tree with no node_modules of its own, such as a test directory.
             That task passes the path back into the install of the source
             tree. Thus the compiled test and the code under test share one
             squint-cljs ESM instance, and they do not resolve two."
  ([dir file] (squint-compile! dir file nil))
  ([dir file opts]
   (let [cmd (if-let [binary (:binary opts)]
               [binary "compile" file]
               ["npx" "squint" "compile" file])
         result (apply tasks/shell {:dir dir :continue true :out :string :err :string} cmd)
         out    (str/replace file (re-pattern "\\.cljc$") ".mjs")]
     (when (seq (:out result)) (print (:out result)))
     (when (seq (:err result)) (print (:err result)))
     (when (not= 0 (:exit result))
       (throw (ex-info (str "squint compile failed: " file)
                       {:exit (:exit result) :err (:err result)})))
     (carry-header! (fs/path dir file) (fs/path dir out))
     (println out "ready at" (str dir "/" out)))))

(defn stage-test-deps!
  "Copy the shipped clj-native helper .mjs files into the test dist
  directory of a consumer. Then a cljs.test mirror imports them by relative
  path, and not through an npm symlink chain.

  The symlink route pulls a second squint-cljs ESM instance into the module
  graph. That splits the cljs.test registry: deftest registers in one
  instance, and run-tests reads the other. The copy keeps every test import
  inside one instance. Each copy still imports 'squint-cljs/...' as a bare
  specifier, which the test node_modules of the consumer resolves.

  opts (optional map):
    :dist        The target directory. The default is \"test/cljc/dist\".
    :files       The file names to stage. The default is platform_state.mjs
                 and test_runner.mjs, the two test-support modules.
    :native-src  The directory that holds the built clj-native .mjs. The
                 default is the checkout that this namespace loaded from. A
                 consumer that runs from the published jar must pass it, for
                 example a node_modules/ffi-wasm path."
  ([] (stage-test-deps! nil))
  ([opts]
   (let [dist (fs/path (or (:dist opts) "test/cljc/dist"))
         native-src (or (some-> (:native-src opts) fs/path)
                        (some-> (clj-native-root)
                                (fs/path "src/cljc/net/willcohen/native"))
                        (throw (ex-info (str "No clj-native checkout on the classpath; "
                                             "pass :native-src (for example a "
                                             "node_modules/ffi-wasm path)")
                                        {})))
         files (or (:files opts) ["platform_state.mjs" "test_runner.mjs"])]
     (fs/create-dirs dist)
     (doseq [n files]
       (let [src (fs/path native-src n)]
         (when-not (fs/exists? src)
           (throw (ex-info (str "Missing clj-native artifact: " src
                                " -- run `bb build:js` in the clj-native tree first")
                           {:missing (str src)})))
         (fs/copy src (fs/path dist n) {:replace-existing true})))
     (println (str "Staged clj-native helpers into " dist
                   ": " (str/join ", " files))))))

(def npm-package-name
  "The npm name that this package publishes under. It is the prefix of every
  bare specifier in export-specifier-rewrites. check-exports-sync! pins it to
  package.json."
  "ffi-wasm")

(def shipped-module-files
  "The file names of every compiled .mjs that the npm tarball ships. This is
  the module inventory of the package, stated as data. Thus a consumer build
  task can stage or rewrite the whole set, and it transcribes nothing.

  A transcribed copy rots when someone adds a module. check-exports-sync!
  pins this list to the files map and the exports map in package.json, and it
  fails build:js on any drift."
  ["dispatch.mjs" "fetch_worker.mjs" "handler_env.mjs" "handler_fs.mjs"
   "handler_heap.mjs" "handler_paths.mjs" "handler_runtime.mjs"
   "http_bridge.mjs" "macros.mjs" "platform_state.mjs" "pool.mjs"
   "test_runner.mjs" "workload_pool.mjs"])

(defn export-specifier-rewrites
  "Map every bare specifier that this package exports, such as
  \"ffi-wasm/pool\", to its module file name behind target-prefix.

  A consumer can copy shipped-module-files adjacent to its own assets. That
  consumer passes the prefix that reaches the copies. The prefix is \"./\"
  for the same directory, and \"./ffi-wasm/\" for a subdirectory. It then
  gives the result to rewrite-import-specifiers!. Thus a module graph with no
  importmap, such as a module worker, resolves the imports.

  The export subpath is the file name with dashes in place of underscores,
  and with no extension. check-exports-sync! pins that rule to package.json."
  [target-prefix]
  (into {}
        (map (fn [f]
               [(str npm-package-name "/"
                     (-> f
                         (str/replace (re-pattern "\\.mjs$") "")
                         (str/replace "_" "-")))
                (str target-prefix f)]))
        shipped-module-files))

(defn rewrite-import-specifiers!
  "Rewrite the ESM import specifiers in the file at `target`, from
  `rewrites`. `rewrites` maps a specifier string to a replacement string.

  This fn covers the three shapes that squint and hand-written modules emit:
  the static `from \"x\"`, the dynamic `import(\"x\")`, and
  `import.meta.resolve(\"x\")`. The whitespace before the opening quote is
  optional, because a minified bundle emits `from\"x\"`.

  Each import keeps its own quote character. Thus a rewrite on squint output
  stays byte-identical outside the specifier itself. This fn writes only
  after a change, and it returns true when it wrote."
  [target rewrites]
  (let [escape-re (fn [s] (str/replace s (re-pattern "[.*+?^${}()|\\[\\]\\\\]") "\\\\$0"))
        content (slurp (fs/file target))
        rewritten
        (reduce
         (fn [s [from to]]
           (let [esc (escape-re from)
                 qto (java.util.regex.Matcher/quoteReplacement to)
                 static-re (re-pattern (str "from\\s*([\"'])" esc "\\1"))
                 dynamic-re (re-pattern (str "import\\(\\s*([\"'])" esc "\\1\\s*\\)"))
                 resolve-re (re-pattern (str "import\\.meta\\.resolve\\(\\s*([\"'])" esc "\\1\\s*\\)"))]
             (-> s
                 (str/replace static-re (str "from $1" qto "$1"))
                 (str/replace dynamic-re (str "import($1" qto "$1)"))
                 (str/replace resolve-re (str "import.meta.resolve($1" qto "$1)")))))
         content
         rewrites)]
    (when (not= content rewritten)
      (spit (fs/file target) rewritten)
      true)))

(def consumer-squint-edn
  "The canonical squint.edn for the squint source directory of a consumer.
  This namespace owns it, because every line is a contract of this package.
  The contract covers the npm name, the src/cljc layout, and the macros.cljc
  that the files list ships. ensure-consumer-squint-edn! stamps it into a
  consumer tree."
  (str ";; GENERATED by clj-native's ensure-consumer-squint-edn! from the\n"
       ";; consumer's own `bb squint`; edit it there, not here.\n"
       ";; Adds the npm-installed ffi-wasm source tree to squint's :paths so\n"
       ";; CLJS macros can :require [net.willcohen.native.macros ...] and pull\n"
       ";; the cross-platform helpers (c-name->clj-name, camel-name->clj-name)\n"
       ";; instead of reimplementing them locally. ffi-wasm's package.json\n"
       ";; `files` list ships macros.cljc, so the path resolves for a consumer\n"
       ";; of the packed tarball as well as for an npm-linked checkout.\n"
       "{:paths [\".\" \"node_modules/ffi-wasm/src/cljc\"]}\n"))

(defn ensure-consumer-squint-edn!
  "Write consumer-squint-edn into `dir` when the file is missing or
  different. Returns true when it wrote. A consumer calls this at the start
  of its primary squint task. Thus the copy in its tree cannot drift from the
  canonical text."
  [dir]
  (let [f (fs/file (str dir) "squint.edn")
        current (when (fs/exists? (fs/path (str dir) "squint.edn")) (slurp f))]
    (when (not= current consumer-squint-edn)
      (spit f consumer-squint-edn)
      (println (str "Wrote canonical squint.edn into " dir))
      true)))

(defn check-exports-sync!
  "Pin npm-package-name, shipped-module-files and the export subpath rule
  behind export-specifier-rewrites to package.json. This fn reads the
  package.json at the repo root that this namespace loaded from. It throws
  with the exact drift when the two disagree. It runs ahead of build:js, thus
  a stale inventory cannot ship."
  []
  (let [root (or (clj-native-root)
                 (throw (ex-info (str "check-exports-sync! needs a checkout; "
                                      "a jar classpath carries no package.json")
                                 {})))
        pkg (json/parse-string (slurp (str (fs/path root "package.json"))))
        pkg-name (get pkg "name")
        files-mjs (->> (get pkg "files")
                       (filter #(str/ends-with? % ".mjs"))
                       (map #(str (fs/file-name %)))
                       set)
        ;; "." is the package entry point. "./package.json" is a plain file
        ;; subpath that strict-exports tooling asks for. Neither one is a
        ;; shipped module, thus neither belongs in the module inventory.
        export-pairs (->> (dissoc (get pkg "exports") "." "./package.json")
                          (map (fn [[sub target]] [(subs sub 2) (str (fs/file-name target))]))
                          set)
        expected-files (set shipped-module-files)
        expected-pairs (->> (export-specifier-rewrites "")
                            (map (fn [[spec f]] [(subs spec (inc (count npm-package-name))) f]))
                            set)]
    (when-not (and (= pkg-name npm-package-name)
                   (= files-mjs expected-files)
                   (= export-pairs expected-pairs))
      (throw (ex-info "build.clj module inventory disagrees with package.json"
                      {:name {:package-json pkg-name :build-clj npm-package-name}
                       :files-only-in-package-json (sort (remove expected-files files-mjs))
                       :files-only-in-build-clj (sort (remove files-mjs expected-files))
                       :exports-only-in-package-json (sort (map vec (remove expected-pairs export-pairs)))
                       :exports-only-in-build-clj (sort (map vec (remove export-pairs expected-pairs)))})))
    (println (str "package.json inventory in sync: " (count expected-files)
                  " modules, " (count expected-pairs) " exports"))))
