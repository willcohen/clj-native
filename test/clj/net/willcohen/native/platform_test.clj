;; Copyright (c) 2026 Will Cohen
;;
;; Part of clj-native, under the Apache License v2.0 with LLVM Exceptions.
;; See LICENSE for license information.
;; SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception

(ns net.willcohen.native.platform-test
  "Tests of the library-agnostic platform helpers. The resolvers run against
   clojure.core. Extraction runs against test/resources with the host pinned
   to linux/amd64, so one fixture path works on every machine."
  (:require [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [net.willcohen.native.platform :as platform]
            [net.willcohen.native.platform-state :as nps]
            [tech.v3.datatype.ffi :as dt-ffi])
  (:import [java.io File]))

(deftest get-os-arch
  ;; Compare with the JVM's own properties, since keyword? alone cannot fail.
  (testing "get-os agrees with os.name on the machine running the suite"
    (let [os-name (.toLowerCase (System/getProperty "os.name"))
          vendor  (.toLowerCase (System/getProperty "java.vendor"))]
      (is (= (cond (.contains vendor "android") :android
                   (.contains os-name "mac")    :darwin
                   (.contains os-name "win")    :windows
                   :else                        :linux)
             (platform/get-os)))))
  (testing "get-arch normalizes os.arch to one of the names the suffix tables use"
    (let [arch (platform/get-arch)]
      (is (contains? #{:amd64 :x86 :aarch64 :arm} arch)
          (str "os.arch " (System/getProperty "os.arch") " normalized to " arch))
      (is (= (case (System/getProperty "os.arch")
               ("amd64" "x86_64" "x86-64") :amd64
               ("i386" "i486" "i586" "i686" "i786" "i886") :x86
               arch)
             arch)))))

(deftest resolve-native-fn-basics
  (testing "resolves an interned Var by keyword key"
    (let [v (platform/resolve-native-fn 'clojure.core :inc)]
      (is (var? v))
      (is (= 2 (@v 1)))))
  (testing "name coercion accepts string keys too"
    (is (var? (platform/resolve-native-fn 'clojure.core "inc"))))
  (testing "returns nil for a missing fn"
    (is (nil? (platform/resolve-native-fn 'clojure.core :no-such-fn-xyzzy)))))

(deftest make-native-fn-resolver-variants
  (testing "bare resolver mirrors resolve-native-fn"
    (let [r (platform/make-native-fn-resolver 'clojure.core)]
      (is (= 2 (@(r :inc) 1)))
      (is (nil? (r :no-such-fn-xyzzy)))))
  (testing ":throw? throws ex-info with fn-key + impl-ns on a miss"
    (let [r (platform/make-native-fn-resolver 'clojure.core {:throw? true})]
      (is (var? (r :inc)))
      (let [e (try (r :no-such-fn-xyzzy) nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e))
        (is (= :no-such-fn-xyzzy (:fn-key (ex-data e))))
        (is (= 'clojure.core (:impl-ns (ex-data e)))))))
  (testing ":memoize? caches the resolved Var per key"
    (let [r (platform/make-native-fn-resolver 'clojure.core {:memoize? true})]
      (is (identical? (r :inc) (r :inc))))))

(deftest call-native-fn-maps-a-null-const-char*-return-to-nil
  ;; dt-ffi throws a PToPointer error for a NULL const char* return. This
  ;; simulates that error, so it cannot show that dt-ffi still throws it.
  (let [thrower (fn [& _]
                  (throw (IllegalArgumentException.
                          "No implementation of method: :->pointer of protocol: #'tech.v3.datatype.ffi/PToPointer found for class: nil")))]
    (is (nil? (platform/apply-native-fn thrower []))
        "PToPointer failure becomes nil"))
  (testing "an unrelated IllegalArgumentException still propagates"
    (let [thrower (fn [& _] (throw (IllegalArgumentException. "bad argument count")))]
      (is (thrown-with-msg? IllegalArgumentException #"bad argument count"
                            (platform/apply-native-fn thrower [])))))
  (testing "call-native-fn resolves the var in impl-ns and applies it"
    (is (= 2 (platform/call-native-fn 'clojure.core :inc [1]))))
  (testing "call-native-fn throws ex-info naming the fn-key and impl-ns on a miss"
    (let [e (try (platform/call-native-fn 'clojure.core :no-such-fn-xyzzy []) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= :no-such-fn-xyzzy (:fn (ex-data e))))
      (is (= 'clojure.core (:impl-ns (ex-data e)))))))

(deftest libname-from-file-strips-ext-and-lib
  (testing "leading lib + extension are removed"
    (is (= "proj" (platform/libname-from-file (File. "/tmp/x/libproj.dylib"))))
    (is (= "gdal" (platform/libname-from-file (File. "/tmp/x/libgdal.so"))))
    (is (= "proj" (platform/libname-from-file (File. "/tmp/x/libproj.a")))))
  (testing "versioned extensions are removed"
    (is (= "z" (platform/libname-from-file (File. "/tmp/x/libz.so.1"))))
    (is (= "z" (platform/libname-from-file (File. "/tmp/x/libz.so.1.3.1")))))
  (testing "lib strips as a prefix only"
    ;; tifflib.dll is the Windows name of libtiff.
    (is (= "tifflib" (platform/libname-from-file (File. "/tmp/x/tifflib.dll"))))
    (is (= "glib-2.0" (platform/libname-from-file (File. "/tmp/x/glib-2.0.so"))))))

(deftest library-lifecycle-nil-singleton-no-op
  (is (nil? (platform/reset-library! nil))))

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
    (testing "an empty argtypes vector survives"
      (is (= [] (get-in rehydrated [:no_args :argtypes]))))
    (testing ":string? becomes a type that dt-ffi knows"
      (let [r (platform/rehydrate-fn-defs
               {:opt {:rettype :string? :argtypes [[:key :string?]]}})]
        (is (= '[[key :pointer?]] (get-in r [:opt :argtypes]))
            "an argument is a nullable pointer; dispatch gives it a C string")
        (is (= :string (get-in r [:opt :rettype])))))
    (testing "the fn-def keys themselves are untouched"
      (is (= #{:one_arg :no_args} (set (keys rehydrated)))))))

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
                                                    :fn-defs-var  #'platform/default-library-suffixes}
                                                   true))))))
  (testing "a lib with no -musl copy falls back to <os>-<arch>"
    (is (= "fake custom-suffix payload\n"
           (slurp (:file (extract-fixture-library! {:lib-basename "libextracttest"
                                                    :suffixes     {:linux ".custom"}
                                                    :fn-defs-var  #'platform/default-library-suffixes}
                                                   true)))))))

(deftest extract-and-bind-library-copies-the-platform-library
  (let [{:keys [file path libname singleton] :as state}
        (extract-fixture-library! {:lib-basename "libextracttest"
                                   :tmp-prefix   "extract-test"
                                   :fn-defs-var  #'platform/default-library-suffixes})]
    (testing "the library is copied out of <os>-<arch>/ under its own name"
      (is (some? file))
      (is (= "libextracttest.so" (.getName ^File file)))
      (is (.exists ^File file))
      (is (= "fake shared object payload\n" (slurp file))))
    (testing ":path is the directory holding the extracted library"
      (is (= path (.getCanonicalPath (.getParentFile ^File file))))
      (is (.isDirectory (File. ^String path))))
    (testing ":libname drops the leading lib and the extension"
      (is (= "extracttest" libname)))
    (testing "a dt-ffi singleton is built over the fn-defs Var"
      (is (some? singleton)))
    (testing "no other keys are returned"
      (is (= #{:file :path :libname :singleton} (set (keys state)))))))

(deftest extract-and-bind-library-honors-a-suffix-override
  (let [{:keys [file]} (extract-fixture-library!
                        {:lib-basename "libextracttest"
                         :suffixes     {:linux ".custom"}
                         :fn-defs-var  #'platform/default-library-suffixes})]
    (is (= "libextracttest.custom" (.getName ^File file)))
    (is (= "fake custom-suffix payload\n" (slurp file)))))

(deftest extract-and-bind-library-copies-extra-resources
  (let [{:keys [path]} (extract-fixture-library!
                        {:lib-basename    "libextracttest"
                         :fn-defs-var     #'platform/default-library-suffixes
                         :extra-resources [{:resource "extract-test-sidecar.dat"}
                                           {:resource "extract-test-sidecar.dat"
                                            :dest     "renamed.dat"}
                                           {:resource-dir "extract-test/"}]})]
    (testing "a single resource lands beside the library under its own name"
      (is (= "sidecar database\n" (slurp (File. ^String path "extract-test-sidecar.dat")))))
    (testing ":dest renames it"
      (is (= "sidecar database\n" (slurp (File. ^String path "renamed.dat")))))
    (testing "a resource directory is copied whole, nesting included"
      (is (= "top level\n" (slurp (File. ^String path "extract-test/a.txt"))))
      (is (= "nested entry\n" (slurp (File. ^String path "extract-test/nested/b.txt")))))))

(deftest extract-and-bind-library-accepts-a-resource-dir-without-a-trailing-slash
  ;; Unnormalized, "extract-test" plus "a.txt" reads as "extract-testa.txt",
  ;; and the whole extraction silently returns {}.
  (let [{:keys [path]} (extract-fixture-library!
                        {:lib-basename    "libextracttest"
                         :fn-defs-var     #'platform/default-library-suffixes
                         :extra-resources [{:resource-dir "extract-test"}]})]
    (is (some? path) "the extraction must not fall back to the empty map")
    (is (= "top level\n" (slurp (File. ^String path "extract-test/a.txt"))))
    (is (= "nested entry\n" (slurp (File. ^String path "extract-test/nested/b.txt"))))))

(deftest extracted-files-are-readable-whatever-the-umask-is
  ;; File.setReadable fails on a path that does not exist yet, so the
  ;; permission calls must follow the copy.
  (let [{:keys [file]} (extract-fixture-library!
                        {:lib-basename "libextracttest"
                         :fn-defs-var  #'platform/default-library-suffixes})]
    (is (.canRead ^File file) "an unreadable library cannot be dlopen'd")
    (is (.canExecute ^File file))))

(deftest extract-and-bind-library-returns-an-empty-map-on-failure
  (testing "a library with no resource for the running platform"
    (is (= {} (extract-fixture-library!
               {:lib-basename "libnosuchlibrary"
                :fn-defs-var  #'platform/default-library-suffixes}))))
  (testing "a missing extra resource fails the whole extraction"
    (is (= {} (extract-fixture-library!
               {:lib-basename    "libextracttest"
                :fn-defs-var     #'platform/default-library-suffixes
                :extra-resources [{:resource "no-such-sidecar.dat"}]}))))
  (testing "an extras entry naming neither a resource nor a resource directory"
    (is (= {} (extract-fixture-library!
               {:lib-basename    "libextracttest"
                :fn-defs-var     #'platform/default-library-suffixes
                :extra-resources [{:dest "nowhere"}]})))))

(deftest try-init-falls-back-to-graal-when-no-lib-was-extracted
  ;; A host with no packaged lib: without the throw, init records :ffi and
  ;; each call fails.
  (let [{:keys [singleton file]} (extract-fixture-library!
                                  {:lib-basename "libnosuchlibrary"
                                   :fn-defs-var  #'platform/default-library-suffixes})
        impl  (atom nil)
        saved System/err]
    (try
      (System/setErr (java.io.PrintStream. (java.io.ByteArrayOutputStream.)))
      (with-out-str
        (nps/try-init! impl (atom false) false
                       #(platform/init-jdk-library! singleton file)
                       (constantly nil)))
      (finally (System/setErr saved)))
    (is (= :graal @impl))))

(def ^:private cleanup-probe
  (str "(require '[net.willcohen.native.platform :as p])"
       "(with-redefs [p/get-os (constantly :linux) p/get-arch (constantly :amd64)"
       "              p/library-dirs (constantly [\"linux-amd64\"])]"
       "  (print (:path (p/extract-and-bind-library!"
       "                 {:lib-basename \"libextracttest\""
       "                  :fn-defs-var #'p/default-library-suffixes"
       "                  :extra-resources [{:resource-dir \"extract-test/\"}]}))))"
       "(flush)"))

(deftest extracted-files-and-dirs-go-at-exit
  (let [{:keys [exit out err]} (sh/sh "clojure" "-Sdeps" "{:paths [\"src/clj\" \"src/cljc\" \"test/resources\"]}"
                                      "-M" "-e" cleanup-probe)]
    (is (= 0 exit) err)
    (is (seq out) "the probe printed the dir")
    (is (not (.exists (File. ^String out))) out)))

(def ^:private demo-fn-defs
  (platform/rehydrate-fn-defs
   {:demo_add     {:rettype :int32 :argtypes [[:lhs :int32] [:rhs :int32]]}
    :demo_name    {:rettype :string :argtypes [[:ctx :pointer]]}
    :demo_checked {:rettype :int32 :argtypes [[:ctx :pointer]] :check-error? true}}))

(def ^:private demo-state
  (atom {:singleton (dt-ffi/library-singleton #'demo-fn-defs)}))

(platform/define-library-fns! demo-fn-defs demo-state)

(defn- emitted-defns
  "Return the defn forms a define-library-fns! call expands to."
  [form]
  (->> (macroexpand form)
       (tree-seq coll? seq)
       (filter #(and (seq? %) (= 'clojure.core/defn (first %))))))

(deftest define-library-fns-interns-one-var-per-fndef
  ;; Reach the Vars as a consumer does, through resolve-native-fn, since they
  ;; do not exist until load time.
  (let [demo-ns 'net.willcohen.native.platform-test
        add     (platform/resolve-native-fn demo-ns :demo_add)
        nm      (platform/resolve-native-fn demo-ns :demo_name)]
    (testing "each fndef key becomes a Var in the calling namespace"
      (is (var? add))
      (is (var? nm))
      (is (var? (platform/resolve-native-fn demo-ns :demo_checked))))
    (testing "the argument list carries the rehydrated names"
      (is (= '([lhs rhs]) (:arglists (meta add))))
      (is (= '([ctx]) (:arglists (meta nm)))))
    (testing "the Var is bound late through the singleton in the state atom"
      ;; The singleton has no bound library, so this dt-ffi error shows the Var
      ;; routes through the state atom.
      (is (thrown-with-msg? Exception #"Library instance not found"
                            (@add 1 2))))))

(deftest define-library-fns-threads-check-error-to-flagged-fndefs-only
  (let [with-checker (pr-str (emitted-defns
                              '(net.willcohen.native.platform/define-library-fns!
                                net.willcohen.native.platform-test/demo-fn-defs
                                demo-state demo-error-check)))
        no-checker   (pr-str (emitted-defns
                              '(net.willcohen.native.platform/define-library-fns!
                                net.willcohen.native.platform-test/demo-fn-defs
                                demo-state)))]
    (testing "the checker is called for the fndef that sets :check-error?"
      (is (= 1 (count (re-seq #"\(demo-error-check " with-checker)))))
    (testing "the two-argument arity passes no checker at all"
      (is (nil? (re-find #"demo-error-check" no-checker))))
    (testing "one defn is emitted per fndef"
      (is (= 3 (count (emitted-defns
                       '(net.willcohen.native.platform/define-library-fns!
                         net.willcohen.native.platform-test/demo-fn-defs
                         demo-state))))))))
