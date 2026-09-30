;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.platform-test
  "Tests of the library-agnostic platform helpers. The resolvers run against
   clojure.core. Extraction runs against test/resources with the host pinned
   to linux/amd64, so one fixture path works on every machine."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.tools.logging.test :as lt]
            [net.willcohen.native.callbacks :as cb]
            [net.willcohen.native.dispatch :as dispatch]
            [net.willcohen.native.platform :as platform]
            [tech.v3.datatype.ffi :as dt-ffi])
  (:import [java.io File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest native-fns-resolve-or-throw
  (let [r (platform/make-native-fn-resolver 'clojure.core)]
    (is (= 2 (@(r :inc) 1)))
    (is (= {:fn-key :no-such-fn-xyzzy :impl-ns 'clojure.core}
           (try (r :no-such-fn-xyzzy) (catch clojure.lang.ExceptionInfo e (ex-data e))))))
  (is (= 2 (platform/call-native-fn 'clojure.core :inc [1]))))

(deftest apply-native-fn-maps-a-null-const-char*-return-to-nil
  ;; dt-ffi throws a PToPointer error for a NULL const char* return. This
  ;; simulates that error, so it cannot show that dt-ffi still throws it.
  (let [thrower (fn [& _]
                  (throw (IllegalArgumentException.
                          "No implementation of method: :->pointer of protocol: #'tech.v3.datatype.ffi/PToPointer found for class: nil")))]
    (is (nil? (#'platform/apply-native-fn thrower []))
        "PToPointer failure becomes nil"))
  (testing "an unrelated IllegalArgumentException still propagates"
    (let [thrower (fn [& _] (throw (IllegalArgumentException. "bad argument count")))]
      (is (thrown-with-msg? IllegalArgumentException #"bad argument count"
                            (#'platform/apply-native-fn thrower []))))))

(deftest rehydrate-fn-defs-turns-argument-names-into-symbols
  (let [rehydrated (platform/rehydrate-fn-defs
                    {:one_arg  {:rettype :int32
                                :argtypes [[:ctx :pointer] [:count :int32]]
                                :doc "kept"}
                     :no_args  {:rettype :void :argtypes []}})]
    (testing "each argument name becomes a symbol"
      (is (= '[[ctx :pointer] [count :int32]]
             (get-in rehydrated [:one_arg :argtypes]))))
    (testing "the rest of each argtype entry and the other fn-def keys survive"
      (is (= :int32 (get-in rehydrated [:one_arg :rettype])))
      (is (= "kept" (get-in rehydrated [:one_arg :doc]))))
    (testing ":string? becomes a type that dt-ffi knows"
      (let [r (platform/rehydrate-fn-defs
               {:opt {:rettype :string? :argtypes [[:key :string?]]}})]
        (is (= '[[key :pointer?]] (get-in r [:opt :argtypes]))
            "an argument is a nullable pointer; dispatch gives it a C string")
        (is (= :string (get-in r [:opt :rettype])))))))

(def ^:private no-fndefs {})

(defn- extract-fixture-library!
  "Run extract-and-bind-library! with host detection pinned to the platform the
   committed fixtures use."
  ([opts] (extract-fixture-library! opts false))
  ([opts musl?]
   (let [dirs (#'platform/library-dirs :linux :amd64 musl?)]
     (with-redefs [platform/get-os       (constantly :linux)
                   platform/get-arch     (constantly :amd64)
                   platform/library-dirs (constantly dirs)]
       (platform/extract-and-bind-library! opts)))))

(deftest musl-maps-names-the-musl-loader
  (is (#'platform/musl-maps?
       "7f3a1c000000-7f3a1c05e000 r-xp 00000000 fd:01 1234 /lib/ld-musl-x86_64.so.1\n"))
  (is (not (#'platform/musl-maps?
            "7f3a1c000000-7f3a1c05e000 r-xp 00000000 fd:01 1234 /usr/lib/x86_64-linux-gnu/libc.so.6\n"))))

(deftest library-dirs-of-this-process
  (testing "a musl process tries the -musl dir first"
    (with-redefs [platform/get-os   (constantly :linux)
                  platform/get-arch (constantly :amd64)]
      (with-redefs-fn {#'platform/musl-process? (delay true)}
        #(is (= ["linux-amd64-musl" "linux-amd64"] (#'platform/library-dirs))))))
  (testing "off Linux, the maps file is never read"
    (with-redefs [platform/get-os   (constantly :darwin)
                  platform/get-arch (constantly :aarch64)]
      (with-redefs-fn {#'platform/musl-process? (delay (throw (ex-info "read" {})))}
        #(is (= ["darwin-aarch64"] (#'platform/library-dirs)))))))

(deftest process-maps-reads-on-linux
  ;; The unit test above cannot see a failed read: the delay turns one into
  ;; "not musl".
  (if (= :linux (platform/get-os))
    (is (re-find #"(?m)^[0-9a-f]+-[0-9a-f]+ " (#'platform/process-maps)))
    (println "SKIP process-maps-reads-on-linux: not on Linux")))

(deftest extract-and-bind-library-takes-the-musl-dir-on-musl
  (testing "the -musl dir wins on musl"
    (is (= "fake musl shared object payload\n"
           (slurp (:file (extract-fixture-library! {:lib-basename "libextracttest"
                                                    :fn-defs-var  #'no-fndefs}
                                                   true))))))
  (testing "a lib with no -musl copy falls back to <os>-<arch>"
    (is (= "fake custom-suffix payload\n"
           (slurp (:file (extract-fixture-library! {:lib-basename "libfallbacktest"
                                                    :fn-defs-var  #'no-fndefs}
                                                   true)))))))

(deftest extract-and-bind-library-copies-the-platform-library
  (let [{:keys [file path]}
        (extract-fixture-library! {:lib-basename "libextracttest"
                                   :tmp-prefix   "extract-test"
                                   :fn-defs-var  #'no-fndefs})]
    (testing "the library is copied out of <os>-<arch>/ under its own name"
      (is (= "libextracttest.so" (.getName ^File file)))
      (is (.exists ^File file))
      (is (= "fake shared object payload\n" (slurp file))))
    (testing ":path is the directory holding the extracted library"
      (is (= path (.getCanonicalPath (.getParentFile ^File file))))
      (is (.isDirectory (File. ^String path))))
    ;; File.setReadable fails on a path that does not exist yet, so the
    ;; permission calls must follow the copy.
    (testing "the library is executable whatever the umask is"
      (is (.canExecute ^File file)))))

(deftest extract-and-bind-library-copies-extra-resources
  (let [{:keys [path]} (extract-fixture-library!
                        {:lib-basename    "libextracttest"
                         :fn-defs-var     #'no-fndefs
                         :extra-resources [{:resource "extract-test-sidecar.dat"}
                                           {:resource-dir "extract-test"}]})]
    (testing "a single resource lands beside the library under its own name"
      (is (= "sidecar database\n" (slurp (File. ^String path "extract-test-sidecar.dat")))))
    (testing "a resource directory is copied whole, nesting included"
      (is (= "top level\n" (slurp (File. ^String path "extract-test/a.txt"))))
      (is (= "nested entry\n" (slurp (File. ^String path "extract-test/nested/b.txt")))))))

(defn- warned-extract-failure?
  "True when the test log holds the extraction warning for `lib-basename`."
  [lib-basename]
  (lt/logged? 'net.willcohen.native.platform :warn Throwable
              (re-pattern (str "Could not extract packaged library " lib-basename " "))))

(deftest extract-and-bind-library-returns-an-empty-map-on-failure
  (testing "a library with no resource for the running platform"
    (lt/with-log
      (is (= {} (extract-fixture-library!
                 {:lib-basename "libnosuchlibrary"
                  :fn-defs-var  #'no-fndefs})))
      (is (warned-extract-failure? "libnosuchlibrary"))))
  (testing "a missing extra resource fails the whole extraction"
    (lt/with-log
      (is (= {} (extract-fixture-library!
                 {:lib-basename    "libextracttest"
                  :fn-defs-var     #'no-fndefs
                  :extra-resources [{:resource "no-such-sidecar.dat"}]})))
      (is (warned-extract-failure? "libextracttest"))))
  (testing "an extras entry naming neither a resource nor a resource directory"
    (lt/with-log
      (is (= {} (extract-fixture-library!
                 {:lib-basename    "libextracttest"
                  :fn-defs-var     #'no-fndefs
                  :extra-resources [{}]})))
      (is (warned-extract-failure? "libextracttest")))))

(deftest init-jdk-library!-throws-when-no-lib-was-extracted
  ;; The throw makes try-init! fall back to GraalVM.
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"No packaged native library"
                        (platform/init-jdk-library! nil nil))))

(deftest delete-tree!-deletes-nested-files-and-empty-dirs
  (let [root (.toFile (Files/createTempDirectory "delete-tree" (make-array FileAttribute 0)))
        f    (File. root "a/b/c.txt")]
    (io/make-parents f)
    (spit f "x")
    (.mkdirs (File. root "empty"))
    (#'platform/delete-tree! root)
    (is (not (.exists root)))))

(def ^:private demo-fn-defs
  (platform/rehydrate-fn-defs
   {:demo_add     {:rettype :int32 :argtypes [[:lhs :int32] [:rhs :int32]]}
    :demo_name    {:rettype :string :argtypes [[:ctx :pointer]]}}))

(def ^:private demo-state
  (atom {:singleton (dt-ffi/library-singleton #'demo-fn-defs)}))

(platform/define-library-fns! demo-fn-defs demo-state)

(deftest define-library-fns-interns-one-var-per-fndef
  ;; Resolve the Vars at run time, since they do not exist until load time.
  (let [add (ns-resolve 'net.willcohen.native.platform-test 'demo_add)
        nm  (ns-resolve 'net.willcohen.native.platform-test 'demo_name)]
    (testing "each fndef key becomes a Var in the calling namespace"
      (is (var? add))
      (is (var? nm)))
    (testing "the argument list carries the rehydrated names"
      (is (= '([lhs rhs]) (:arglists (meta add))))
      (is (= '([ctx]) (:arglists (meta nm)))))
    (testing "the Var is bound late through the singleton in the state atom"
      ;; The singleton has no bound library, so this dt-ffi error shows the Var
      ;; routes through the state atom.
      (is (thrown-with-msg? Exception #"Library instance not found"
                            (@add 1 2))))))

(defn- class-files-left-by
  "Call f with *compile-path* bound to a new temporary directory. Return the
   .class files that f leaves below it."
  [f]
  (let [dir (.toFile (Files/createTempDirectory "clj-native-compile-path"
                                                (into-array FileAttribute [])))]
    (try
      (binding [*compile-path* (.getPath dir)]
        (f))
      (filterv #(.endsWith (.getName ^File %) ".class") (file-seq dir))
      (finally
        (#'platform/delete-tree! dir)))))

(defn- system-c-library
  "The C library of this process, or nil on an OS that this test does not
   know."
  ^File []
  (case (platform/get-os)
    :darwin (File. "/usr/lib/libSystem.B.dylib")
    :linux  (some-> (re-find #"(?m)/\S*/libc\.so\.6$" (#'platform/process-maps))
                    (File.))
    nil))

(def ^:private libc-fn-defs
  (platform/rehydrate-fn-defs
   {:abs {:rettype :int32 :argtypes [[:x :int32]]}}))

(deftest init-jdk-library!-leaves-no-class-file-in-the-compile-path
  (if-let [libc (system-c-library)]
    (let [singleton (dt-ffi/library-singleton #'libc-fn-defs)]
      (is (empty? (class-files-left-by
                   #(platform/init-jdk-library! singleton libc))))
      (is (some? @singleton) "the library is bound"))
    (println "SKIP init-jdk-library!-leaves-no-class-file-in-the-compile-path:"
             "no known C library on this OS")))

(defn- system-math-library
  "The C math library of this process, or nil on an OS that this test does
   not know. macOS keeps libm in libSystem."
  ^File []
  (case (platform/get-os)
    :darwin (File. "/usr/lib/libSystem.B.dylib")
    :linux  (some-> (re-find #"(?m)/\S*/libm\.so\.6$" (#'platform/process-maps))
                    (File.))
    nil))

(def ^:private math-fndefs
  {:fabsf {:rettype :float32 :argtypes [[:x :float32]]}})

(def ^:private math-fn-defs (platform/rehydrate-fn-defs math-fndefs))

(def ^:private math-state
  (atom {:singleton (dt-ffi/library-singleton #'math-fn-defs)}))

(platform/define-library-fns! math-fn-defs math-state)

(deftest call!-on-ffi-passes-and-returns-a-float32
  (if-let [libm (system-math-library)]
    (let [lib (dispatch/library {:key ::math
                                 :fndefs math-fndefs
                                 :impl-atom (atom :ffi)
                                 :ffi-impl-ns 'net.willcohen.native.platform-test})]
      (platform/init-jdk-library! (:singleton @math-state) libm)
      (is (= (double (float 0.1)) (dispatch/call! lib :fabsf [-0.1]))
          "the argument narrows to f32, and the f32 result comes back"))
    (println "SKIP call!-on-ffi-passes-and-returns-a-float32:"
             "no known C math library on this OS")))

(deftest define-callback-interface-leaves-no-class-file-in-the-compile-path
  (dt-ffi/set-ffi-impl! :jdk)
  (let [iface (atom nil)]
    (is (empty? (class-files-left-by
                 #(reset! iface (cb/define-callback-interface :int64 [:int64])))))
    (is (some? @iface) "the interface is defined")))
