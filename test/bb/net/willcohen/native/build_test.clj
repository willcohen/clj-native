;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.build-test
  "Tests of the babashka build helpers. bb test:bb runs them."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [babashka.tasks :as tasks]
            [clojure.test :refer [deftest is testing]]
            [net.willcohen.native.build :as nb]
            [org.httpkit.server :as http]))

(def ^:private body "hello")

(def ^:private body-sha256
  "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")

(defn- with-server
  "Call `f` with the URL of a local server that answers each request with
   `body`."
  [f]
  (let [stop (http/run-server (fn [_] {:status 200 :body body})
                              {:ip "127.0.0.1" :port 0})]
    (try (f (str "http://127.0.0.1:" (:local-port (meta stop)) "/a.tar.gz"))
         (finally (stop)))))

(deftest download-archive-checks-the-sha256
  (with-server
    (fn [url]
      (let [dest (str (fs/path (fs/create-temp-dir) "a.tar.gz"))]
        (testing "the expected sha256 keeps the file"
          (nb/download-archive url dest {:sha256 body-sha256})
          (is (= body (slurp dest))))
        (testing "a partial file from an earlier run fails the check and goes"
          (spit dest "hel")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"sha256"
                                (nb/download-archive url dest {:sha256 body-sha256})))
          (is (not (fs/exists? dest))))
        (testing "the sha256 compares in any case"
          (nb/download-archive url dest {:sha256 (str/upper-case body-sha256)})
          (is (= body (slurp dest))))))))

(defn- with-truncating-server
  "Call `f` with the URL of a local server that sends 5 of the 10 bytes its
   Content-Length promises, then closes."
  [f]
  (let [ss  (java.net.ServerSocket. 0 1 (java.net.InetAddress/getLoopbackAddress))
        srv (future
              (with-open [sock (.accept ss)]
                (let [in (java.io.BufferedReader. (java.io.InputStreamReader. (.getInputStream sock)))]
                  (loop [] (when (seq (.readLine in)) (recur)))
                  (doto (.getOutputStream sock)
                    (.write (.getBytes "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhello"))
                    (.flush)))))]
    (try (f (str "http://127.0.0.1:" (.getLocalPort ss) "/a.tar.gz"))
         (finally (future-cancel srv) (.close ss)))))

(deftest download-archive-leaves-no-partial-file
  ;; A partial file would pass as the archive on each later run.
  (with-truncating-server
    (fn [url]
      (let [dest (str (fs/path (fs/create-temp-dir) "a.tar.gz"))]
        (is (thrown? Exception (nb/download-archive url dest)))
        (is (not (fs/exists? dest)))))))

(deftest extract-archive-leaves-no-partial-dir
  ;; A partial dir would pass as the source tree on each later run.
  (let [base    (fs/create-temp-dir)
        archive (fs/path base "p.tar.gz")
        rnd     (java.util.Random. 1)]
    (fs/create-dirs (fs/path base "p"))
    (doseq [f ["a" "b"]
            :let [b (byte-array 200000)]]
      (.nextBytes rnd b)
      (fs/write-bytes (fs/path base "p" f) b))
    (tasks/shell {:dir (str base)} "tar" "czf" "p.tar.gz" "p")
    (let [whole (fs/read-all-bytes archive)]
      (fs/write-bytes archive (java.util.Arrays/copyOf whole (quot (count whole) 2))))
    (fs/delete-tree (fs/path base "p"))
    (is (thrown? Exception (nb/extract-archive "p.tar.gz" "p" base)))
    (is (not (fs/exists? (fs/path base "p"))))))

(deftest build-once-runs-again-when-the-args-change
  (let [out    (str (fs/path (fs/create-temp-dir) "lib" "libx.a"))
        runs   (atom 0)
        build! #(do (swap! runs inc)
                    (fs/create-dirs (fs/parent out))
                    (spit out "x"))]
    (nb/build-once! out {:cflags "-O2"} build!)
    (nb/build-once! out {:cflags "-O2"} build!)
    (is (= 1 @runs) "the same args keep the output")
    (nb/build-once! out {:cflags "-O3"} build!)
    (is (= 2 @runs) "new args build again")
    (fs/delete out)
    (nb/build-once! out {:cflags "-O3"} build!)
    (is (= 3 @runs) "a missing output builds again")
    (testing "an old output does not pass for a build that makes none"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"made no"
                            (nb/build-once! out {:cflags "-O0"} (fn []))))
      (is (not (fs/exists? (str out ".build-args.edn"))))
      (nb/build-once! out {:cflags "-O0"} build!)
      (is (= 4 @runs)))))

(deftest build-once-rejects-args-that-do-not-print-as-edn
  (let [out (str (fs/path (fs/create-temp-dir) "libx.a"))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"EDN"
                          (nb/build-once! out {:dir (fs/path "x")} #(spit out "x"))))))

(deftest autotools-rejects-cflags-with-no-optimization-level
  ;; The check runs before the build touches a dir.
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"CFLAGS \"-DX\" has no -O"
                        (nb/build-autotools-library {:type :native :build-dir "/nonexistent"
                                                     :install-dir "/nonexistent" :configure-args []
                                                     :env {"CFLAGS" "-DX" "CXXFLAGS" "-O2"}})))
  (let [check! #'nb/check-cflags!]
    (doseq [flags ["" "-DSQLITE_ENABLE_RTREE"]]
      (is (thrown? clojure.lang.ExceptionInfo (check! {"CFLAGS" flags} {}))
          (str "CFLAGS \"" flags "\" replaces the -O2 of configure")))
    (doseq [flags ["-O2 -DX" "-DX -Os" "-O0" "-Og" "-Ofast -DX"]]
      (is (nil? (check! {"CFLAGS" flags} {})) (str "CFLAGS \"" flags "\" has a level")))
    ;; A nix shell can export CFLAGS, and configure then sees that value.
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"CFLAGS \"-fPIC\""
                          (check! {} {"CFLAGS" "-fPIC"})))
    (is (nil? (check! {} {"CFLAGS" "-O2 -fPIC"})))
    (is (nil? (check! {"CFLAGS" "-O2"} {"CFLAGS" "-fPIC"})) "env wins over the inherited value")))

(defn- on-path? [cmd]
  (some? (fs/which cmd)))

(deftest zig-toolchain-writes-a-wrapper-for-each-tool
  (doseq [[dir target host system] [["linux-aarch64" "aarch64-linux-gnu.2.28" "aarch64-linux-gnu" "Linux"]
                                    ["linux-amd64-musl" "x86_64-linux-musl" "x86_64-linux-musl" "Linux"]
                                    ["windows-amd64" "x86_64-windows-gnu" "x86_64-w64-mingw32" "Windows"]]
          :let [{:keys [env cmake-args] :as zig} (nb/zig-toolchain! dir (fs/create-temp-dir))]]
    (is (= host (:host zig)) "a --host that config.sub knows")
    (is (str/includes? (slurp (get env "CC")) (str "-target " target)))
    (is (every? #(str/includes? (slurp (get env %)) "-fsanitize-trap=undefined") ["CC" "CXX"]))
    (is (every? #(fs/executable? (get env %)) ["CC" "CXX" "AR" "RANLIB"]))
    (is (some #{(str "-DCMAKE_SYSTEM_NAME=" system)} cmake-args))
    (is (some #{(str "-DCMAKE_SYSTEM_PROCESSOR=" (first (str/split target #"-")))} cmake-args))
    (is (some #{(str "-DCMAKE_CXX_COMPILER=" (get env "CXX"))} cmake-args)))
  (is (contains? (:env (nb/zig-toolchain! "windows-amd64" (fs/create-temp-dir))) "LD")
      "libtool archives with lib.exe when LD is not a GNU ld")
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No zig target"
                        (nb/zig-toolchain! "linux-riscv64" (fs/create-temp-dir)))))

(deftest zig-toolchain-hides-the-pkg-config-files-of-the-build-machine
  (if-not (on-path? "pkg-config")
    (println "SKIP zig-toolchain-hides-the-pkg-config-files-of-the-build-machine: no pkg-config on the PATH")
    (let [{:keys [env]} (nb/zig-toolchain! "linux-amd64" (fs/create-temp-dir))]
      (is (not= 0 (:exit (tasks/shell {:extra-env env :continue true :out :string :err :string}
                                      "pkg-config" "--exists" "zlib")))))))

(def ^:private glibc-dynamic
  " 0x0000000000000001 (NEEDED)             Shared library: [libm.so.6]
 0x0000000000000001 (NEEDED)             Shared library: [libc.so.6]
 0x000000000000000e (SONAME)             Library soname: [libproj.so.25]")

(defn- versions [& vs]
  (apply str (map #(str "  0x0010:   Name: " % "  Flags: none  Version: 2\n") vs)))

(defn- header [machine]
  (str "  Type:                              DYN (Shared object file)\n"
       "  Machine:                           " machine "\n"))

(def ^:private amd64 (header "Advanced Micro Devices X86-64"))
(def ^:private aarch64 (header "AArch64"))

(deftest readelf-errors-names-each-host-dependency
  (testing "a zig glibc lib"
    (is (empty? (#'nb/readelf-errors "linux-amd64" amd64 glibc-dynamic
                                       (str (versions "GLIBC_2.2.5" "GLIBC_2.28")
                                            "  000:   0 (*local*)       2 (GLIBC_2.2.5)\n")))))
  (testing "a glibc newer than the target"
    (is (= ["needs GLIBC_2.38"]
           (#'nb/readelf-errors "linux-amd64" amd64 glibc-dynamic (versions "GLIBC_2.2.5" "GLIBC_2.38")))))
  (testing "a gcc build: libstdc++ and libgcc_s"
    (is (= ["NEEDED libstdc++.so.6" "needs GLIBCXX_3.4.30" "needs GCC_3.0"]
           (#'nb/readelf-errors
            "linux-aarch64" aarch64
            " 0x0000000000000001 (NEEDED)             Shared library: [libstdc++.so.6]"
            (versions "GLIBCXX_3.4.30" "GCC_3.0")))))
  (testing "a GLIBC_ need with no version number"
    (is (= ["needs GLIBC_ABI_DT_RELR"]
           (#'nb/readelf-errors "linux-amd64" amd64 glibc-dynamic (versions "GLIBC_2.2.5" "GLIBC_ABI_DT_RELR")))))
  (testing "libutil and libresolv, which each glibc host has"
    (is (empty? (#'nb/readelf-errors
                 "linux-amd64" amd64
                 (str glibc-dynamic
                      "\n 0x0000000000000001 (NEEDED)             Shared library: [libutil.so.1]"
                      "\n 0x0000000000000001 (NEEDED)             Shared library: [libresolv.so.2]")
                 (versions "GLIBC_2.2.5")))))
  (testing "a zig musl lib"
    (is (empty? (#'nb/readelf-errors
                 "linux-amd64-musl" amd64
                 " 0x0000000000000001 (NEEDED)             Shared library: [libc.so]" ""))))
  (testing "a glibc lib in a musl dir, and a run path"
    (is (= ["NEEDED libm.so.6" "NEEDED libc.so.6" "RUNPATH /nix/store/x/lib" "needs GLIBC_2.17"]
           (#'nb/readelf-errors
            "linux-aarch64-musl" aarch64
            (str glibc-dynamic "\n 0x000000000000001d (RUNPATH)            Library runpath: [/nix/store/x/lib]")
            (versions "GLIBC_2.17")))))
  (testing "an object that is not a shared lib, or is for another machine"
    (is (= ["Type REL, not DYN"]
           (#'nb/readelf-errors "linux-amd64"
                                  (str/replace amd64 "DYN (Shared object file)" "REL (Relocatable file)")
                                  glibc-dynamic (versions "GLIBC_2.2.5"))))
    (is (= ["Machine AArch64, not Advanced Micro Devices X86-64"]
           (#'nb/readelf-errors "linux-amd64" aarch64 glibc-dynamic (versions "GLIBC_2.2.5"))))))

(deftest check-linux-lib-accepts-a-zig-lib-for-its-dir
  ;; Needs zig and readelf.
  (if-not (and (on-path? "zig") (on-path? "readelf"))
    (println "SKIP check-linux-lib-accepts-a-zig-lib-for-its-dir: no zig or readelf on the PATH")
    (let [tmp (fs/create-temp-dir)
          src (str (fs/path tmp "x.c"))
          lib (fn [dir]
                (let [{:keys [env]} (nb/zig-toolchain! dir (fs/path tmp "bin"))
                      out (str (fs/path tmp (str dir ".so")))]
                  (tasks/shell (get env "CC") "-O2" "-fPIC" "-shared" "-o" out src)
                  out))]
      (spit src "#include <math.h>\n#include <sys/stat.h>\nint x(const char *p) { struct stat s; return stat(p, &s) + (int)sqrt(4.0); }\n")
      (doseq [dir ["linux-amd64" "linux-amd64-musl"]]
        (is (nil? (nb/check-linux-lib! dir (lib dir))) dir))
      (let [{:keys [env]} (nb/zig-toolchain! "linux-amd64" (fs/path tmp "bin"))
            out (str (fs/path tmp "debug.so"))]
        (tasks/shell (get env "CC") "-O0" "-fPIC" "-shared" "-o" out src)
        (is (not (str/includes? (:out (tasks/shell {:out :string} "readelf" "-Ws" out)) "__ubsan"))
            "a -O0 lib carries no UBSan runtime"))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"does not fit linux-amd64-musl \(NEEDED libc\.so\.6, needs GLIBC_"
                            (nb/check-linux-lib! "linux-amd64-musl" (str (fs/path tmp "linux-amd64.so"))))
          "a glibc lib in a musl dir"))))

(deftest zig-cmake-args-find-nothing-on-the-host
  ;; A host prefix, as a nix shell or a distro exports it, must not reach a
  ;; zig build: its libs and headers are for the build machine.
  (if-not (on-path? "cmake")
    (println "SKIP zig-cmake-args-find-nothing-on-the-host: no cmake on the PATH")
    (let [tmp  (fs/create-temp-dir)
          host (fs/path tmp "host")
          {:keys [env cmake-args]} (nb/zig-toolchain! "linux-amd64" (fs/path tmp "bin"))]
      (fs/create-dirs (fs/path host "lib" "pkgconfig"))
      (fs/create-dirs (fs/path host "include"))
      (spit (str (fs/path host "lib" "libz.so")) "")
      (spit (str (fs/path host "include" "zlib.h")) "")
      (spit (str (fs/path host "lib" "pkgconfig" "zlib.pc"))
            (str "Name: zlib\nDescription: z\nVersion: 1.0\nLibs: -L" host "/lib -lz\n"))
      ;; NONE: no compiler, so no zig.
      (spit (str (fs/path tmp "CMakeLists.txt"))
            (str "cmake_minimum_required(VERSION 3.20)\nproject(p NONE)\n"
                 "find_library(Z_LIB NAMES libz.so)\nfind_path(Z_INC zlib.h)\n"
                 "find_package(PkgConfig)\npkg_check_modules(Z_PC QUIET zlib)\n"
                 "message(STATUS \"found ${Z_LIB} ${Z_INC} zlib.pc=${Z_PC_FOUND}.\")\n"))
      (let [out (:out (apply tasks/shell {:dir (str tmp) :out :string
                                          :extra-env (assoc env "CMAKE_PREFIX_PATH" (str host))}
                             "cmake" "-S" "." "-B" "build" cmake-args))]
        (is (str/includes? out "found Z_LIB-NOTFOUND Z_INC-NOTFOUND zlib.pc=.") out)))))

(def ^:private zig-dll
  "llvm-readobj --file-headers --coff-imports, cut, of a zig DLL."
  "Format: COFF-x86-64
ImageFileHeader {
  Machine: IMAGE_FILE_MACHINE_AMD64 (0x8664)
  Characteristics [ (0x2022)
    IMAGE_FILE_DLL (0x2000)
    IMAGE_FILE_EXECUTABLE_IMAGE (0x2)
  ]
}
Import {
  Name: KERNEL32.dll
  Symbol: GetLastError (0)
}
Import {
  Name: api-ms-win-crt-runtime-l1-1-0.dll
  Symbol: _initterm (0)
}
")

(defn- importing [readobj & dlls]
  (apply str readobj (map #(str "Import {\n  Name: " % "\n  Symbol: f (0)\n}\n") dlls)))

(deftest readobj-errors-names-each-dll-outside-windows
  (is (empty? (#'nb/readobj-errors zig-dll)))
  (is (empty? (#'nb/readobj-errors (importing zig-dll "SHELL32.dll")))
      "PROJ reads the user dir through SHELL32")
  (is (= ["imports libstdc++-6.dll" "imports libgcc_s_seh-1.dll"]
         (#'nb/readobj-errors (importing zig-dll "libstdc++-6.dll" "libgcc_s_seh-1.dll")))
      "a mingw gcc build")
  (is (= ["not a DLL"]
         (#'nb/readobj-errors (str/replace zig-dll "IMAGE_FILE_DLL (0x2000)" ""))))
  (is (= ["Machine IMAGE_FILE_MACHINE_ARM64, not IMAGE_FILE_MACHINE_AMD64"]
         (#'nb/readobj-errors (str/replace zig-dll "AMD64 (0x8664)" "ARM64 (0xAA64)")))))

(deftest check-windows-lib-accepts-a-zig-dll
  ;; Needs zig and llvm-readobj.
  (if-not (and (on-path? "zig") (on-path? "llvm-readobj"))
    (println "SKIP check-windows-lib-accepts-a-zig-dll: no zig or llvm-readobj on the PATH")
    (let [tmp (fs/create-temp-dir)
          src (str (fs/path tmp "x.cpp"))
          lib (fn [dir out & args]
                (let [{:keys [env]} (nb/zig-toolchain! dir (fs/path tmp dir))
                      out (str (fs/path tmp out))]
                  (apply tasks/shell (get env "CXX") "-O2" "-shared" "-o" out src args)
                  out))]
      (spit src (str "#include <stdexcept>\n#include <string>\n"
                     "extern \"C\" int x(const char *s) {\n"
                     "  try { if (std::string(s).empty()) throw std::runtime_error(\"e\"); }\n"
                     "  catch (...) { return -1; }\n  return 1;\n}\n"))
      (is (nil? (nb/check-windows-lib! "windows-amd64" (lib "windows-amd64" "x.dll"))))
      ;; The last -O wins. -O0 is the level of a CMake Debug build.
      (is (nil? (nb/check-windows-lib! "windows-amd64" (lib "windows-amd64" "x0.dll" "-O0")))
          "a -O0 DLL")
      (spit src (str "#include <winsock2.h>\n"
                     "extern \"C\" int y(void) { return WSAGetLastError(); }\n"))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"(?i)imports ws2_32\.dll"
                            (nb/check-windows-lib! "windows-amd64" (lib "windows-amd64" "y.dll" "-lws2_32")))
          "a DLL outside the allowlist"))))

(deftest emcc-link-cmd-sets-the-flag-of-each-option
  (let [cmd (fn [opts]
              (set (#'nb/emcc-link-cmd (merge {:output-name "m.js" :objects ["a.o"]
                                               :exported-functions ["_f"]}
                                              opts))))]
    (is (some #(str/includes? % "\"wasmBinary\"") (cmd {})) "a GraalVM host passes wasmBinary")
    (is (contains? (cmd {}) "GROWABLE_ARRAYBUFFERS=0"))
    (is (contains? (cmd {}) "STACK_SIZE=1048576"))
    (is (contains? (cmd {}) "ENVIRONMENT=web,worker,node,shell"))
    (is (contains? (cmd {:pthreads? true}) "ENVIRONMENT=web,worker,node"))
    (is (contains? (cmd {:pthreads? true}) "PTHREAD_POOL_DELAY_LOAD=1"))
    (is (contains? (cmd {:force-filesystem? true}) "FORCE_FILESYSTEM=1"))))

(deftest check-exports-sync-compares-full-paths
  (let [copy   (str (fs/path (fs/create-temp-dir) "package.json"))
        pkg    (slurp "package.json")
        check! (fn [from to]
                 (is (str/includes? pkg from))
                 (spit copy (str/replace pkg from to))
                 (nb/check-exports-sync! copy))]
    (is (nil? (check! "" "")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (check! "\"./handler\": \"./dist/handler.mjs\"" "\"./handler\": \"./dist/ffi-wasm.mjs\""))
        "a subpath at the wrong dist file")
    (is (thrown? clojure.lang.ExceptionInfo
                 (check! "\"main\": \"dist/ffi-wasm.mjs\""
                         "\"main\": \"src/cljc/net/willcohen/native/pool.mjs\""))
        "a wrong main")
    (is (thrown? clojure.lang.ExceptionInfo
                 (check! "\"src/cljc/net/willcohen/native/macros.cljc\",\n" ""))
        "macros.cljc left out of files")
    (is (thrown? clojure.lang.ExceptionInfo
                 (check! "\"dist/test_runner.mjs\",\n" ""))
        "a dist file left out of files")))

(deftest export-specifier-rewrites-point-each-subpath-at-its-dist-file
  (let [r (nb/export-specifier-rewrites "./ffi-wasm/")]
    (is (= "./ffi-wasm/ffi-wasm.mjs" (get r "ffi-wasm")))
    (is (= "./ffi-wasm/ffi-wasm.mjs" (get r "ffi-wasm/pool")))
    (is (= "./ffi-wasm/ffi-wasm.mjs" (get r "ffi-wasm/platform-state")))
    (is (= "./ffi-wasm/handler.mjs" (get r "ffi-wasm/handler")))
    (is (= "./ffi-wasm/handler_env.mjs" (get r "ffi-wasm/handler-env")))
    (is (= "./ffi-wasm/http_bridge.mjs" (get r "ffi-wasm/http-bridge")))
    (is (= "./ffi-wasm/fetch_worker.mjs" (get r "ffi-wasm/fetch-worker")))
    (is (= "./ffi-wasm/test_runner.mjs" (get r "ffi-wasm/test-runner")))))

(deftest stage-test-deps-copies-each-helper-or-throws
  (let [src  (fs/create-temp-dir)
        dist (str (fs/path (fs/create-temp-dir) "dist"))
        opts {:dist dist :native-src (str src)}]
    (spit (str (fs/path src "platform_state.mjs")) "p")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"test_runner\.mjs"
                          (nb/stage-test-deps! opts)))
    (spit (str (fs/path src "test_runner.mjs")) "t")
    (nb/stage-test-deps! opts)
    (is (= ["p" "t"] (map #(slurp (str (fs/path dist %))) ["platform_state.mjs" "test_runner.mjs"])))))

(def ^:private dylib-deps
  "/tmp/libproj.dylib:
\t@rpath/libproj.25.dylib (compatibility version 25.0.0, current version 25.9.9)
\t/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1351.0.0)
\t/usr/lib/libc++.1.dylib (compatibility version 1.0.0, current version 1900.180.0)
")

(deftest otool-errors-names-each-non-system-dependency
  (is (empty? (#'nb/otool-errors "@rpath/libproj.25.dylib" dylib-deps "")))
  (is (= ["loads /Users/me/My Libs/libz.dylib" "LC_RPATH /Users/me/My Libs"]
         (#'nb/otool-errors
          "@rpath/libproj.25.dylib"
          (str dylib-deps "\t/Users/me/My Libs/libz.dylib (compatibility version 1.0.0, current version 1.3.1)\n")
          "         path /Users/me/My Libs (offset 12)\n"))
      "a path with a space"))

(deftest check-darwin-lib-rejects-a-universal-dylib
  ;; otool of the dev shell reads only the host slice, so another slice could
  ;; hide a run path. A resource dir holds one arch.
  (let [f (fs/file (fs/create-temp-dir) "fat.dylib")]
    (with-open [out (java.io.FileOutputStream. f)]
      (.write out (byte-array (map unchecked-byte [0xCA 0xFE 0xBA 0xBE 0 0 0 2]))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"universal"
                          (nb/check-darwin-lib! (str f))))))

(deftest check-darwin-lib-rejects-a-run-path
  ;; Needs a Mac, for cc and otool.
  (if-not (and (= "Mac OS X" (System/getProperty "os.name")) (on-path? "otool") (on-path? "cc"))
    (println "SKIP check-darwin-lib-rejects-a-run-path: not on a Mac")
    (let [tmp (fs/create-temp-dir)
          src (str (fs/path tmp "x.c"))
          lib (fn [name & flags]
                (let [out (str (fs/path tmp name))]
                  (apply tasks/shell "cc" "-shared" "-o" out src flags)
                  out))]
      (spit src "int x(void) { return 1; }\n")
      (is (nil? (nb/check-darwin-lib! (lib "plain.dylib"))))
      (let [dep (lib "libdep.dylib" (str "-Wl,-install_name," tmp "/libdep.dylib"))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"loads .*/libdep\.dylib, LC_RPATH /tmp/build"
                              (nb/check-darwin-lib! (lib "user.dylib" dep "-Wl,-rpath,/tmp/build"))))))))
