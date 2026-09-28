;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.build
  "Build primitives that a clj-native consumer calls from its bb tasks.
  The consumer supplies the config of its library. Babashka only."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.tasks :as tasks]
            [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn detect-host-platform
  "Returns {:os :darwin|:linux|:windows|:unknown
            :arch :aarch64|:amd64|:unknown}."
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

(defn- safe-parallel-jobs
  "A make -j value: one job per 3 GB of memory (available on Linux, total
  on macOS), from 1 to the CPU count, so a C++ build fits a small CI
  runner."
  []
  (let [gb (try
             (if (fs/exists? "/proc/meminfo")
               (-> (re-find #"MemAvailable:\s+(\d+)" (slurp "/proc/meminfo"))
                   second parse-long (/ 1048576.0))
               (-> (tasks/shell {:out :string} "sysctl" "-n" "hw.memsize")
                   :out str/trim parse-long (/ 1073741824.0)))
             (catch Exception _ 8.0))]
    (min (.availableProcessors (Runtime/getRuntime)) (max 1 (int (/ gb 3))))))

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
  "The command prefix of `tool`: the emscripten wrapper for :wasm, the bare
  tool for :native."
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
  "Build a library with configure, make and make install. Throws first
  when CFLAGS or CXXFLAGS has no -O level (check-cflags!).

  Required keys:
    :type             :native, or :wasm to wrap the tools in emconfigure
                      and emmake.
    :build-dir        Deleted and created again, unless :in-tree?.
    :install-dir      The --prefix. Deleted before the build.
    :configure-args   For example [\"--disable-shared\"].

  Optional keys:
    :env              More env vars, such as CC and CFLAGS.
    :configure-script Relative to :build-dir. Default \"../configure\", or
                      \"./configure\" with :in-tree?.
    :in-tree?         :build-dir is the source dir. Runs make distclean
                      first when a Makefile exists.
    :post-configure   A fn of no args, called between configure and make."
  [{:keys [type build-dir install-dir configure-args env
           configure-script in-tree? post-configure]
    :or {env {}}}]
  (check-cflags! env (System/getenv))
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
    (apply tasks/shell {:dir (str build-dir) :extra-env env}
           (concat (cmd-prefix type :configure)
                   [configure-script (str "--prefix=" install-dir)]
                   configure-args))
    (when post-configure (post-configure))
    (apply tasks/shell {:dir (str build-dir) :extra-env env}
           (concat make-cmd ["-j" (str (safe-parallel-jobs))]))
    (apply tasks/shell {:dir (str build-dir) :extra-env env}
           (concat make-cmd ["install"]))))

(defn build-cmake-library
  "Build a library with CMake.

  Required keys:
    :type           :native, or :wasm to wrap the tools in emcmake and
                    emmake.
    :src-dir        The directory with CMakeLists.txt.
    :build-dir      The out-of-tree build directory.
    :cmake-args     Every CMake flag, with each dep path.

  Optional keys:
    :env            More env vars.
    :clean?         Delete :build-dir first. Default true. False keeps the
                    cache for an incremental rebuild."
  [{:keys [type src-dir build-dir cmake-args env clean?]
    :or {clean? true
         env {}}}]
  (when clean? (fs/delete-tree build-dir))
  (fs/create-dirs build-dir)
  (let [jobs      (str (safe-parallel-jobs))
        build-cmd (if (= type :wasm)
                    (concat (cmd-prefix type :make) ["-j" jobs])
                    ["cmake" "--build" "." "--parallel" jobs])]
    (apply tasks/shell {:dir (str build-dir) :extra-env env}
           (concat (cmd-prefix type :cmake) cmake-args [(str src-dir)]))
    (apply tasks/shell {:dir (str build-dir) :extra-env env} build-cmd)))

(def ^:private emscripten-incoming-module-js-api-default
  "A copy of the emscripten default INCOMING_MODULE_JS_API, from
  src/settings.js. The flag replaces the default, so emcc-link passes this
  whole list. Sync it at each emscripten bump."
  ["ENVIRONMENT" "arguments" "canvas" "dynamicLibraries" "elementPointerLock"
   "instantiateWasm" "locateFile" "monitorRunDependencies" "noExitRuntime"
   "noInitialRun" "onAbort" "onExit" "onRuntimeInitialized" "postRun"
   "preInit" "preRun" "print" "printErr" "setStatus" "statusMessage"
   "stderr" "stdin" "stdout" "thisProgram" "wasm" "websocket"])

(def ^:private max-glibc
  "A glibc lib loads on this glibc and later."
  "2.28")

(def ^:private zig-targets
  "The zig target of each resource dir that zig builds."
  {"linux-amd64"        (str "x86_64-linux-gnu." max-glibc)
   "linux-aarch64"      (str "aarch64-linux-gnu." max-glibc)
   "linux-amd64-musl"   "x86_64-linux-musl"
   "linux-aarch64-musl" "aarch64-linux-musl"
   "windows-amd64"      "x86_64-windows-gnu"})

(defn- write-tool-wrapper!
  "Write an sh script at `path` that runs zig with `args`, then its own
  arguments. configure and CMake take one executable."
  [path args]
  (spit (str path) (str "#!/bin/sh\nexec zig " (str/join " " args) " \"$@\"\n"))
  (fs/set-posix-file-permissions path "rwxr-xr-x"))

(defn- zig-host
  "The configure --host of a zig target: no glibc version, and for Windows
  the mingw name, since config.sub does not know the zig one."
  [target]
  (-> target
      (str/replace #"\.[0-9.]+$" "")
      (str/replace #"-windows-gnu$" "-w64-mingw32")))

(defn zig-toolchain!
  "Write zig wrappers for the resource `dir` into bin-dir. Returns :env for
  configure or CMake, :host for configure --host, and :cmake-args that
  search no dir of the build machine."
  [dir bin-dir]
  (let [target   (or (zig-targets dir)
                     (throw (ex-info (str "No zig target for " dir)
                                     {:dir dir :known (keys zig-targets)})))
        windows? (str/includes? target "windows")
        host   (zig-host target)
        tool   #(str (fs/absolutize (fs/path bin-dir (str host "-" %))))
        tools  (cond-> {"CC" "cc" "CXX" "c++" "AR" "ar" "RANLIB" "ranlib"}
                 ;; libtool takes a non-GNU LD, such as ld64, for MSVC and
                 ;; archives with lib.exe. zig's lld passes as a GNU ld.
                 windows? (assoc "LD" "ld.lld"))]
    (fs/create-dirs bin-dir)
    (doseq [[_ t] tools]
      (write-tool-wrapper! (tool t) (if (#{"cc" "c++"} t) [t "-target" target] [t])))
    ;; An empty pkg-config search, as the CMake root below: a .pc file of
    ;; the build machine would link its libs, whatever their license. The
    ;; nixpkgs wrapper replaces PKG_CONFIG_PATH with a _FOR_TARGET or
    ;; _FOR_BUILD path.
    {:env        (assoc (update-vals tools tool)
                        "PKG_CONFIG_LIBDIR" ""
                        "PKG_CONFIG_PATH" ""
                        "PKG_CONFIG_PATH_FOR_BUILD" ""
                        "PKG_CONFIG_PATH_FOR_TARGET" "")
     :host       host
     :cmake-args [(str "-DCMAKE_SYSTEM_NAME=" (if windows? "Windows" "Linux"))
                  (str "-DCMAKE_SYSTEM_PROCESSOR=" (first (str/split host #"-")))
                  (str "-DCMAKE_C_COMPILER=" (tool "cc"))
                  (str "-DCMAKE_CXX_COMPILER=" (tool "c++"))
                  (str "-DCMAKE_AR=" (tool "ar"))
                  (str "-DCMAKE_RANLIB=" (tool "ranlib"))
                  ;; With no root, ONLY mode still searches the build
                  ;; machine. bin-dir holds only the wrappers.
                  (str "-DCMAKE_FIND_ROOT_PATH=" (fs/absolutize bin-dir))
                  "-DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM=NEVER"
                  "-DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY=ONLY"
                  "-DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY"
                  "-DCMAKE_FIND_ROOT_PATH_MODE_PACKAGE=ONLY"
                  ;; FindPkgConfig adds the .pc dir of each CMAKE_PREFIX_PATH
                  ;; entry, with no root.
                  "-DPKG_CONFIG_USE_CMAKE_PREFIX_PATH=OFF"]}))

(def ^:private glibc-libs
  "The host glibc libraries that a glibc lib may need."
  #{"libc.so.6" "libm.so.6" "libpthread.so.0" "libdl.so.2" "librt.so.1"
    "libutil.so.1" "libresolv.so.2" "ld-linux-x86-64.so.2" "ld-linux-aarch64.so.1"})

(def ^:private elf-machines
  "The `readelf -h` Machine of each arch of the Linux resource dirs."
  {"amd64" "Advanced Micro Devices X86-64" "aarch64" "AArch64"})

(defn- version<=
  "True when the dotted version `a` is at most `b`."
  [a b]
  (let [parse #(mapv parse-long (str/split % #"\."))
        [va vb] [(parse a) (parse b)]
        n (max (count va) (count vb))
        pad #(into % (repeat (- n (count %)) 0))]
    (<= (compare (pad va) (pad vb)) 0)))

(defn- readelf-errors
  "The errors in the `readelf -h`, `-d` and `-V` output of a lib for the
  Linux resource `dir`. A glibc lib may need only glibc up to max-glibc, a
  musl lib only libc.so. Neither may need libstdc++ or libgcc, or have a
  run path."
  [dir header dynamic-section version-info]
  (let [musl?   (str/ends-with? dir "-musl")
        ok-lib? (if musl? #{"libc.so"} glibc-libs)
        want    (elf-machines (second (str/split dir #"-")))
        type    (second (re-find #"(?m)^\s*Type:\s+(\S+)" header))
        machine (second (re-find #"(?m)^\s*Machine:\s+(.+?)\s*$" header))]
    (concat
     (when (not= "DYN" type) [(str "Type " type ", not DYN")])
     (when (not= want machine) [(str "Machine " machine ", not " want)])
     (for [[_ lib] (re-seq #"\(NEEDED\)\s+Shared library: \[([^\]]+)\]" dynamic-section)
           :when (not (ok-lib? lib))]
       (str "NEEDED " lib))
     (for [[_ tag path] (re-seq #"\((RUNPATH|RPATH)\)\s+Library r\w*path: \[([^\]]*)\]" dynamic-section)]
       (str tag " " path))
     (for [[v n] (distinct (re-seq #"\bGLIBC_([A-Za-z0-9_.]+)" version-info))
           :when (or musl? (not (re-matches #"[0-9.]+" n)) (not (version<= n max-glibc)))]
       (str "needs " v))
     (for [v (distinct (re-seq #"\b(?:GLIBCXX|CXXABI|GCC)_[0-9.]+" version-info))]
       (str "needs " v)))))

(defn check-linux-lib!
  "Throw when readelf-errors finds an error in `lib` for the Linux resource
  `dir`. Needs readelf on the PATH."
  [dir lib]
  (let [readelf #(:out (tasks/shell {:out :string} "readelf" % (str lib)))]
    (when-let [errors (seq (readelf-errors dir (readelf "-h") (readelf "-d") (readelf "-V")))]
      (throw (ex-info (str lib " does not fit " dir " (" (str/join ", " errors) "). "
                           "Build it with zig-toolchain!.")
                      {:lib (str lib) :dir dir :errors errors})))
    (println "OK" (str lib) "fits" dir)))

(defn- system-dll?
  "True for a DLL of each Windows 10 and later: KERNEL32, SHELL32 and the
  api-ms-win-crt sets of the UCRT. DLL names ignore case."
  [dll]
  (let [dll (str/lower-case dll)]
    (boolean (or (#{"kernel32.dll" "shell32.dll"} dll)
                 (re-matches #"api-ms-win-crt-[a-z0-9-]+\.dll" dll)))))

(defn- readobj-errors
  "The errors in the `llvm-readobj --file-headers --coff-imports` output of
  a DLL for windows-amd64: not a DLL, another machine, or an import that is
  not a system-dll?, such as a mingw runtime."
  [readobj]
  (let [want    "IMAGE_FILE_MACHINE_AMD64"
        machine (second (re-find #"(?m)^\s*Machine: (\S+)" readobj))]
    (concat
     (when-not (re-find #"\bIMAGE_FILE_DLL\b" readobj) ["not a DLL"])
     (when (not= want machine) [(str "Machine " machine ", not " want)])
     (for [dll (distinct (map second (re-seq #"(?m)^\s*Name: (\S+)" readobj)))
           :when (not (system-dll? dll))]
       (str "imports " dll)))))

(defn check-windows-lib!
  "Throw when readobj-errors finds an error in the DLL `lib` for the
  Windows resource `dir`. Needs llvm-readobj on the PATH."
  [dir lib]
  (let [readobj (:out (tasks/shell {:out :string} "llvm-readobj"
                                   "--file-headers" "--coff-imports" (str lib)))]
    (when-let [errors (seq (readobj-errors readobj))]
      (throw (ex-info (str lib " does not fit " dir " (" (str/join ", " errors) "). "
                           "Build it with zig-toolchain!.")
                      {:lib (str lib) :dir dir :errors errors})))
    (println "OK" (str lib) "fits" dir)))

(defn- otool-errors
  "The errors in the `otool -L` and `otool -l` output of the dylib with
  install name `id`: a non-system library, or a run path, which names a dir
  of the build machine."
  [id deps-output load-commands]
  (concat
   (for [[_ dep] (re-seq #"(?m)^\s+(.+?) \(compatibility" deps-output)
         :when (not (or (= dep id)
                        (str/starts-with? dep "/usr/lib/")
                        (str/starts-with? dep "/System/Library/")))]
     (str "loads " dep))
   (for [[_ path] (re-seq #"(?m)^\s+path (.+?) \(offset" load-commands)]
     (str "LC_RPATH " path))))

(defn- universal-binary?
  "True when the file at `lib` starts with the magic of a universal Mach-O."
  [lib]
  (with-open [in (io/input-stream (fs/file lib))]
    (let [b (byte-array 4)]
      (and (= 4 (.read in b))
           (contains? #{0xCAFEBABE 0xCAFEBABF}
                      (reduce #(+ (* %1 256) (bit-and %2 0xff)) 0 b))))))

(defn check-darwin-lib!
  "Throw when the dylib `lib` is universal, or otool-errors finds an error
  in it."
  [lib]
  (when (universal-binary? lib)
    (throw (ex-info (str lib " is a universal binary. Each resource dir takes a thin dylib.")
                    {:lib (str lib)})))
  (let [otool #(:out (tasks/shell {:out :string} "otool" % (str lib)))
        id    (last (str/split-lines (otool "-D")))]
    (when-let [errors (seq (otool-errors id (otool "-L") (otool "-l")))]
      (throw (ex-info (str lib " does not load the same on each Mac (" (str/join ", " errors) ").")
                      {:lib (str lib) :errors errors})))
    (println "OK" (str lib) "loads only system libraries and has no run path")))

(defn- js-string-list
  "Render names as an emcc -s list, for example [\"ccall\",\"getValue\"]."
  [coll]
  (str "[" (str/join "," (map (fn [s] (str "\"" s "\"")) coll)) "]"))

(defn- emcc-link-cmd
  [{:keys [output-name objects exported-functions exported-runtime-methods
           pthreads? environment extra-flags module-name allow-memory-growth?
           optimization maximum-memory force-filesystem?]
    :or {module-name "Module"
         allow-memory-growth? true
         optimization "-O2"}}]
  ;; worker-router runs each module in a Web Worker, so even a
  ;; single-threaded build needs `worker`.
  (let [environment (or environment (if pthreads? "web,worker,node" "web,worker,node,shell"))]
    (vec (concat ["em++" "-o" output-name optimization
                  "-fexceptions" "-fvisibility=default"
                  "--target=wasm32-unknown-emscripten" "--no-entry"]
                 (when pthreads? ["-pthread"])
                 extra-flags
                 (when force-filesystem? ["-s" "FORCE_FILESYSTEM=1"])
                 (map str objects)
                 [(str "-sEXPORTED_FUNCTIONS=" (js-string-list exported-functions))]
                 (when (seq exported-runtime-methods)
                   [(str "-sEXPORTED_RUNTIME_METHODS=" (js-string-list exported-runtime-methods))])
                 ;; A GraalVM host gives the wasm bytes through wasmBinary.
                 [(str "-sINCOMING_MODULE_JS_API="
                       (js-string-list (conj emscripten-incoming-module-js-api-default
                                             "wasmBinary")))]
                 ["-s" (str "ENVIRONMENT=" environment)]
                 (when allow-memory-growth? ["-s" "ALLOW_MEMORY_GROWTH=1"])
                 ;; At 1 the resizable memory fails TextDecoder.decode in Chrome and Firefox,
                 ;; so each string return throws. The emscripten default varies by version.
                 (when allow-memory-growth? ["-s" "GROWABLE_ARRAYBUFFERS=0"])
                 ["-s" "ALLOW_TABLE_GROWTH=1"]
                 (when maximum-memory ["-s" (str "MAXIMUM_MEMORY=" maximum-memory)])
                 ["-s" "MODULARIZE=1"
                  "-s" "EXPORT_ES6=1"
                  "-s" (str "EXPORT_NAME=\"" module-name "\"")]
                 ;; Deep C++ code can overflow the 64K default, and that
                 ;; corrupts dlmalloc with no error. Each pthread stack
                 ;; also gets STACK_SIZE.
                 ["-s" "STACK_SIZE=1048576"]
                 (when pthreads? ["-s" "FETCH=1"])
                 ;; One pool worker serves a rare pthread_create, as from
                 ;; libcxx <thread>. DELAY_LOAD keeps 'loading-workers'
                 ;; out of init, which would outlast the worker-router
                 ;; bootstrap timeout. Disable such calls at the source too, as
                 ;; with -DSQLITE_MAX_WORKER_THREADS=0.
                 (when pthreads?
                   ["-s" "USE_PTHREADS=1"
                    "-s" "SHARED_MEMORY=1"
                    "-s" "PTHREAD_POOL_SIZE=1"
                    "-s" "PTHREAD_POOL_DELAY_LOAD=1"])))))

(defn emcc-link
  "Link objects into a WASM module with em++. Writes the .js and .wasm into
  :build-dir. Throws on a non-zero exit.

  Required keys:
    :build-dir          em++ runs here.
    :output-name        For example \"mylib.js\".
    :objects            Object files and static libs, in link order.
    :exported-functions C names, each with a leading underscore.

  Optional keys:
    :exported-runtime-methods For example [\"ccall\" \"cwrap\"].
    :pthreads?          -pthread, USE_PTHREADS, SHARED_MEMORY and FETCH, with
                        one pool worker that loads on first use. Default
                        false.
    :environment        Default \"web,worker,node\", plus \",shell\" without
                        :pthreads?.
    :force-filesystem?  Keep the FS runtime, which emcc drops when no
                        compiled call site uses it. Set it when the consumer
                        stages files at run time. Default false.
    :extra-flags        emcc flags, placed before the objects.
    :module-name        EXPORT_NAME. Default \"Module\".
    :allow-memory-growth? Default true.
    :maximum-memory     Bytes. Default unset.
    :optimization       Default \"-O2\".

  The module is an ES6 MODULARIZE factory with a 1 MiB stack and a growable
  table."
  [opts]
  (apply tasks/shell {:dir (str (:build-dir opts))} (emcc-link-cmd opts)))

(defn emcc-compile
  "Compile the C or C++ file :source to the object file :output with
  `emcc -c -O3`, with -I for each of :include-dirs. Throws on a non-zero
  exit."
  [{:keys [source output include-dirs]}]
  (apply tasks/shell "emcc" "-c" (str source) "-o" (str output) "-O3" "-fexceptions"
         (mapcat (fn [d] ["-I" (str d)]) include-dirs)))

(defn- leading-comment-block
  "The run of `;;` lines at the top of `source` as `//` lines, or nil."
  [source]
  (let [lines (take-while (fn [l] (str/starts-with? l ";;"))
                          (str/split-lines source))]
    (when (seq lines)
      (str (str/join "\n" (map (fn [l] (str "//" (subs l 2))) lines))
           "\n\n"))))

(defn- carry-header!
  "Prepend the leading comment block of `src-file` to `out-file`, unless
  `out-file` starts with it already."
  [src-file out-file]
  (when-let [header (leading-comment-block (slurp (fs/file src-file)))]
    (let [body (slurp (fs/file out-file))]
      (when-not (str/starts-with? body header)
        (spit (fs/file out-file) (str header body))))))

(defn squint-compile!
  "Compile the .cljc or .cljs `file` to the .mjs beside it, in `dir`. Throws on a
  non-zero exit. Copies the leading `;;` header onto the .mjs, since squint
  drops comments and the license header must ship.

  opts:
    :binary  A squint executable, relative to `dir`. Default `npx squint`.
             A test tree with no node_modules passes the squint of the
             source tree, so test and code share one squint-cljs instance."
  ([dir file] (squint-compile! dir file nil))
  ([dir file opts]
   (let [cmd (if-let [binary (:binary opts)]
               [binary "compile" file]
               ["npx" "squint" "compile" file])
         result (apply tasks/shell {:dir dir :continue true :out :string :err :string} cmd)
         out    (str/replace file (re-pattern "\\.clj[cs]$") ".mjs")]
     (when (seq (:out result)) (print (:out result)))
     (when (seq (:err result)) (print (:err result)))
     (when (not= 0 (:exit result))
       (throw (ex-info (str "squint compile failed: " file)
                       {:exit (:exit result) :err (:err result)})))
     (carry-header! (fs/path dir file) (fs/path dir out))
     (println out "ready at" (str dir "/" out)))))

(defn stage-test-deps!
  "Copy platform_state.mjs and test_runner.mjs, the clj-native test
  helpers, from the dir :native-src into :dist (default
  \"test/cljc/dist\"), for import by relative path. An npm symlink would
  load a second squint-cljs instance, and deftest and run-tests would then
  see different registries. Throws when a file is missing.

  Each copy still imports squint-cljs by bare specifier, so the consumer's
  test node_modules must have it."
  [{:keys [native-src dist] :or {dist "test/cljc/dist"}}]
  (let [files ["platform_state.mjs" "test_runner.mjs"]]
    (fs/create-dirs dist)
    (doseq [n files]
      (let [src (fs/path (str native-src) n)]
        (when-not (fs/exists? src)
          (throw (ex-info (str "Missing clj-native artifact: " src
                               ". Install ffi-wasm where :native-src points.")
                          {:missing (str src)})))
        (fs/copy src (fs/path dist n) {:replace-existing true})))
    (println (str "Staged clj-native helpers into " dist ": " (str/join ", " files)))))

(def ^:private npm-package-name
  "The npm package name. check-exports-sync! pins it to package.json."
  "ffi-wasm")

(def shipped-module-files
  "Each compiled .mjs that the npm tarball ships. check-exports-sync! pins
  it to package.json."
  ;; macros.mjs is dead code in a consumer bundle, but esbuild needs it to
  ;; resolve the import.
  ["dispatch.mjs" "fetch_worker.mjs" "handler_env.mjs" "handler_fs.mjs"
   "handler_heap.mjs" "handler_paths.mjs" "handler_runtime.mjs"
   "http_bridge.mjs" "macros.mjs" "platform_state.mjs" "pool.mjs"
   "test_runner.mjs" "workload_pool.mjs"])

(defn export-specifier-rewrites
  "Map each bare specifier this package exports, such as \"ffi-wasm/pool\",
  to `target-prefix` plus its file name. For rewrite-import-specifiers! on
  copies of shipped-module-files, where no importmap applies. The subpath is
  the file name with dashes for underscores and no extension."
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
  "Rewrite the import specifiers in the file at `target` by `rewrites`, a
  map of specifier to replacement. Covers `from \"x\"` and
  `import.meta.resolve(\"x\")`, with or without a space before the quote,
  and keeps each quote character. Returns true when it wrote the file."
  [target rewrites]
  (let [content (slurp (fs/file target))
        rewritten
        (reduce
         (fn [s [from to]]
           (let [esc (java.util.regex.Pattern/quote from)
                 qto (java.util.regex.Matcher/quoteReplacement to)
                 static-re (re-pattern (str "from\\s*([\"'])" esc "\\1"))
                 resolve-re (re-pattern (str "import\\.meta\\.resolve\\(\\s*([\"'])" esc "\\1\\s*\\)"))]
             (-> s
                 (str/replace static-re (str "from $1" qto "$1"))
                 (str/replace resolve-re (str "import.meta.resolve($1" qto "$1)")))))
         content
         rewrites)]
    (when (not= content rewritten)
      (spit (fs/file target) rewritten)
      true)))

(def ^:private consumer-squint-edn
  "The squint.edn that ensure-consumer-squint-edn! writes into a consumer.
  It lives here because its path depends on the npm name and layout of this
  package."
  (str ";; Written by net.willcohen.native.build/ensure-consumer-squint-edn!.\n"
       ";; The ffi-wasm path lets a CLJS macro require net.willcohen.native.macros.\n"
       "{:paths [\".\" \"node_modules/ffi-wasm/src/cljc\"]}\n"))

(defn ensure-consumer-squint-edn!
  "Write consumer-squint-edn into `dir` when the squint.edn there differs.
  Returns true when it wrote. Call it at the start of the squint task."
  [dir]
  (let [f (fs/file (str dir) "squint.edn")
        current (when (fs/exists? (fs/path (str dir) "squint.edn")) (slurp f))]
    (when (not= current consumer-squint-edn)
      (spit f consumer-squint-edn)
      (println (str "Wrote canonical squint.edn into " dir))
      true)))

(defn check-exports-sync!
  "Throw with the drift when the file `package-json` disagrees with
  npm-package-name and shipped-module-files: its name, main, each exports
  entry and its files, all by full path."
  [package-json]
  (let [pkg  (json/parse-string (slurp (str package-json)))
        dir  "src/cljc/net/willcohen/native/"
        main (str dir "handler_runtime.mjs")
        want {"name"    npm-package-name
              "main"    main
              "exports" (into {"." (str "./" main) "./package.json" "./package.json"}
                              (map (fn [[spec f]] [(str "." (subs spec (count npm-package-name))) f]))
                              (export-specifier-rewrites (str "./" dir)))
              ;; squint reads macros.cljc to expand the macros.
              "files"   (sort (cons (str dir "macros.cljc") (map #(str dir %) shipped-module-files)))}
        have  (update (select-keys pkg (keys want)) "files" sort)
        drift (into {} (remove (fn [[k v]] (= v (get have k)))) want)]
    (when (seq drift)
      (throw (ex-info "package.json disagrees with the module inventory of net.willcohen.native.build"
                      {:build-clj drift :package-json (select-keys have (keys drift))})))
    (println (str "package.json inventory in sync: " (count shipped-module-files) " modules"))))
