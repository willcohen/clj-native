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
  "Write the tool wrappers for the zig target of `dir` into bin-dir.
  Returns :env (CC, CXX, AR, RANLIB, and LD for Windows), :host (for
  configure --host) and :cmake-args. The CMake args find nothing on the
  build machine, so give CMake each dependency by its path.

  zig links libc++, libc++abi, libunwind and compiler-rt (Apache-2.0 WITH
  LLVM-exception) into the lib and libc dynamically, so the lib holds no
  libstdc++ or libgcc. A Windows lib imports the UCRT of Windows 10 and
  later."
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
                  ;; ONLY searches nothing but the root. With no root, CMake
                  ;; searches the build machine. bin-dir holds only the
                  ;; wrappers.
                  (str "-DCMAKE_FIND_ROOT_PATH=" (fs/absolutize bin-dir))
                  "-DCMAKE_FIND_ROOT_PATH_MODE_PROGRAM=NEVER"
                  "-DCMAKE_FIND_ROOT_PATH_MODE_LIBRARY=ONLY"
                  "-DCMAKE_FIND_ROOT_PATH_MODE_INCLUDE=ONLY"
                  "-DCMAKE_FIND_ROOT_PATH_MODE_PACKAGE=ONLY"
                  ;; FindPkgConfig adds the .pc dir of each CMAKE_PREFIX_PATH
                  ;; entry, with no root.
                  "-DPKG_CONFIG_USE_CMAKE_PREFIX_PATH=OFF"]}))

(def ^:private glibc-libs
  "The glibc libraries, from the host, that a lib for a glibc dir may need."
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

(defn- clj-native-root
  "The clj-native checkout root, or nil when clj-native loads from a jar."
  []
  (let [resource (io/resource "net/willcohen/native/build.clj")]
    (when (= "file" (some-> resource .getProtocol))
      ;; <root>/src/bb/net/willcohen/native/build.clj
      (str (nth (iterate fs/parent (fs/path (.toURI resource))) 6)))))

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
