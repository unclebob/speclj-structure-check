(ns speclj-structure-check.core-spec
  (:require [speclj.core :refer :all]
            [speclj-structure-check.core :as pc]))

(describe "scan"
  (it "returns OK for empty string"
    (should= {:errors [] :depth 0 :forms []} (pc/scan "")))

  (it "tracks paren depth"
    (should= 0 (:depth (pc/scan "(foo)")))
    (should= 1 (:depth (pc/scan "(foo")))
    (should= 0 (:depth (pc/scan "(foo (bar))"))))

  (it "ignores parens inside strings"
    (should= 0 (:depth (pc/scan "(def x \"(((\")"))))

  (it "ignores parens inside comments"
    (should= 0 (:depth (pc/scan "(def x 1) ; ((("))))

  (it "handles escaped quotes in strings"
    (should= 0 (:depth (pc/scan "(def x \"a\\\"b\")"))))

  (it "ignores parens inside regex literals"
    (should= 0 (:depth (pc/scan "(def x #\"(((\")"))))

  (it "ignores character literal parens"
    (should= 0 (:depth (pc/scan "(def x \\( \\))"))))

  (it "hash not followed by quote does not enter regex mode"
    (should= 0 (:depth (pc/scan "(def x #{1 2 3})"))))

  (it "handles multi-line strings with parens"
    (should= 0 (:depth (pc/scan "(def x \"line1(\nline2)\")"))))

  (it "tracks line number correctly after comment"
    (let [result (pc/scan "; comment\n(describe \"x\")")]
      (should= 2 (-> result :forms first :line))))

  (it "handles escaped quote inside regex"
    (should= 0 (:depth (pc/scan "(def x #\"a\\\"b\")"))))

  (it "tracks line number correctly across multi-line regex"
    (let [result (pc/scan "(def x #\"a\nb\")\n(describe \"y\")")]
      (should= 3 (-> result :forms first :line))))

  (it "character literal backslash does not escape next char"
    (should= 0 (:depth (pc/scan "(def x \\\\)"))))

  (it "backslash before quote in normal mode escapes the quote"
    (should= 0 (:depth (pc/scan "(def x \\\")"))))

  (it "hash followed by non-quote does not enter regex mode"
    (should= 0 (:depth (pc/scan "(def x #(inc %))"))))

  (it "non-hash before quote does not enter regex mode"
    (should= 0 (:depth (pc/scan "(def x \"a\")")))))

(describe "private helpers"
  (it "treats a plain quote as string mode, not regex mode"
    (should= {:mode :string}
             (#'pc/handle-normal-char {:mode :normal} \" nil)))

  (it "treats hash quote as regex mode and skips the quote"
    (should= {:mode :regex :skip true}
             (#'pc/handle-normal-char {:mode :normal} \# \")))

  (it "clears skip without re-enabling it"
    (should= false (:skip (#'pc/process-char {:skip true} \x nil))))

  (it "clears escape without re-enabling it"
    (should= false (:escape (#'pc/process-char {:escape true} \x nil))))

  (it "extracts a token through end of input"
    (should= "foo" (#'pc/extract-token [\( \f \o \o] 0)))

  (it "returns nil when there is no token after an opening paren"
    (should= nil (#'pc/extract-token [\( \)] 0))
    (should= nil (#'pc/extract-token [\(] 0)))

  (it "does not push a form when depth did not increase"
    (let [state {:line 1 :errors [] :form-stack []}
          chars [\( \d \e \s \c \r \i \b \e]]
      (should= state (#'pc/maybe-push-form state chars 0 0 0))))

  (it "pushes a valid speclj form onto the stack"
    (let [state {:line 3 :errors [] :form-stack []}
          chars [\( \d \e \s \c \r \i \b \e]]
      (should= [{:form "describe" :line 3 :depth 0}]
               (:form-stack (#'pc/maybe-push-form state chars 0 0 1)))))

  (it "does not mark a form completed when depth is unchanged"
    (let [state {:form-stack [{:depth 1 :form "it" :line 2}]}]
      (should-not (#'pc/completed-form? state \) 1 1 :normal))))

  (it "returns tree opts only when tree flag is set"
    (should= {} (#'pc/tree-opts #{}))
    (should= {:tree true} (#'pc/tree-opts #{"--tree"})))

  (it "returns an empty tree for empty forms"
    (should= "" (pc/format-tree [])))

  (it "prints missing paths as failures"
    (let [out (with-out-str
                (should-not (#'pc/print-missing-path "/tmp/nope.clj")))]
      (should-contain "/tmp/nope.clj: not found" out))))

(describe "speclj form tracking"
  (it "detects describe form"
    (let [result (pc/scan "(describe \"foo\")")]
      (should= [{:form "describe" :line 1}] (:forms result))))

  (it "detects describe with it children"
    (let [result (pc/scan "(describe \"foo\"\n  (it \"bar\"))")]
      (should= [{:form "describe" :line 1
                 :children [{:form "it" :line 2}]}]
               (:forms result))))

  (it "detects context inside describe"
    (let [result (pc/scan "(describe \"foo\"\n  (context \"ctx\"\n    (it \"bar\")))")]
      (should= [{:form "describe" :line 1
                 :children [{:form "context" :line 2
                             :children [{:form "it" :line 3}]}]}]
               (:forms result))))

  (it "detects before and with-stubs"
    (let [result (pc/scan "(describe \"x\"\n  (before (reset!))\n  (with-stubs)\n  (it \"y\"))")]
      (should= 3 (count (:children (first (:forms result)))))))

  (it "detects multiple top-level describe blocks"
    (let [result (pc/scan "(describe \"a\"\n  (it \"x\"))\n(describe \"b\"\n  (it \"y\"))")]
      (should= 2 (count (:forms result)))
      (should= "describe" (-> result :forms first :form))
      (should= 1 (-> result :forms first :line))
      (should= 3 (-> result :forms second :line))))

  (it "does not track non-speclj forms inside it"
    (let [result (pc/scan "(describe \"x\"\n  (it \"y\"\n    (let [a 1]\n      (should= 1 a))))")]
      (should= 0 (count (:errors result)))
      (should= 1 (count (:children (first (:forms result))))))))

(describe "error detection"
  (it "detects (it) inside (it)"
    (let [result (pc/scan "(describe \"x\"\n  (it \"outer\"\n    (it \"inner\")))")]
      (should= 1 (count (:errors result)))
      (should-contain "line 3" (first (:errors result)))
      (should-contain "(it) inside (it)" (first (:errors result)))))

  (it "detects (describe) inside (describe)"
    (let [result (pc/scan "(describe \"x\"\n  (describe \"y\"))")]
      (should= 1 (count (:errors result)))))

  (it "allows (context) inside (context)"
    (let [result (pc/scan "(describe \"x\"\n  (context \"a\"\n    (context \"b\")))")]
      (should= 0 (count (:errors result)))))

  (it "detects (describe) inside (it)"
    (let [result (pc/scan "(describe \"x\"\n  (it \"y\"\n    (describe \"z\")))")]
      (should= 1 (count (:errors result)))))

  (it "detects (before) inside (it)"
    (let [result (pc/scan "(describe \"x\"\n  (it \"y\"\n    (before (reset!))))")]
      (should= 1 (count (:errors result)))))

  (it "detects (context) inside (it)"
    (let [result (pc/scan "(describe \"x\"\n  (it \"y\"\n    (context \"z\")))")]
      (should= 1 (count (:errors result)))))

  (it "no error for correct nesting"
    (let [result (pc/scan "(describe \"x\"\n  (before (reset!))\n  (it \"a\")\n  (it \"b\"))")]
      (should= 0 (count (:errors result)))))

  (it "no error for context with its"
    (let [result (pc/scan "(describe \"x\"\n  (context \"ctx\"\n    (it \"a\")\n    (it \"b\")))")]
      (should= 0 (count (:errors result)))))

  (it "detects (describe) inside (context)"
    (let [result (pc/scan "(describe \"x\"\n  (context \"c\"\n    (describe \"y\")))")]
      (should= 1 (count (:errors result)))
      (should-contain "(describe) inside (context)" (first (:errors result)))))

  (it "detects (with-stubs) inside (it)"
    (let [result (pc/scan "(describe \"x\"\n  (it \"y\"\n    (with-stubs)))")]
      (should= 1 (count (:errors result)))))

  (it "detects (around) inside (it)"
    (let [result (pc/scan "(describe \"x\"\n  (it \"y\"\n    (around [f] (f))))")]
      (should= 1 (count (:errors result)))))

  (it "collects multiple errors in one file"
    (let [result (pc/scan "(describe \"x\"\n  (it \"a\"\n    (it \"b\"))\n  (it \"c\"\n    (it \"d\")))")]
      (should= 2 (count (:errors result)))
      (should-contain "line 3" (first (:errors result)))
      (should-contain "line 5" (second (:errors result)))))

  (it "reports unclosed form at EOF"
    (let [result (pc/scan "(describe \"x\"\n  (it \"y\"")]
      (should (<= 1 (count (:errors result))))
      (should-contain "unclosed" (first (:errors result))))))

(describe "format-tree"
  (it "formats a single describe with children"
    (let [forms [{:form "describe" :line 1
                  :children [{:form "before" :line 2}
                             {:form "it" :line 3}
                             {:form "it" :line 5}]}]
          result (pc/format-tree forms)]
      (should= (str "(describe :line 1\n"
                    "  (before :line 2)\n"
                    "  (it :line 3)\n"
                    "  (it :line 5))")
               result)))

  (it "formats nested context"
    (let [forms [{:form "describe" :line 1
                  :children [{:form "context" :line 2
                              :children [{:form "it" :line 3}]}]}]
          result (pc/format-tree forms)]
      (should= (str "(describe :line 1\n"
                    "  (context :line 2\n"
                    "    (it :line 3)))")
               result)))

  (it "formats leaf node"
    (should= "(describe :line 1)" (pc/format-tree [{:form "describe" :line 1}]))))

(describe "check-file"
  (it "returns OK for an empty file"
    (let [tmp (java.io.File/createTempFile "empty-spec" ".clj")
          path (.getAbsolutePath tmp)]
      (.deleteOnExit tmp)
      (spit path "")
      (should= "OK" (pc/check-file path))))

  (it "returns errors for bad structure"
    (let [tmp (java.io.File/createTempFile "bad-spec" ".clj")
          path (.getAbsolutePath tmp)]
      (.deleteOnExit tmp)
      (spit path "(describe \"x\"\n  (it \"a\"\n    (it \"b\")))")
      (let [result (pc/check-file path)]
        (should-not= "OK" result)
        (should-contain "ERROR" result))))

  (it "appends tree when :tree option is true"
    (let [tmp (java.io.File/createTempFile "tree-spec" ".clj")
          path (.getAbsolutePath tmp)]
      (.deleteOnExit tmp)
      (spit path "(describe \"x\"\n  (it \"a\")\n  (it \"b\"))")
      (let [result (pc/check-file path {:tree true})]
        (should-contain "OK" result)
        (should-contain "(describe :line 1" result)
        (should-contain "(it :line 2)" result))))

  (it "appends tree after errors when :tree option is true"
    (let [tmp (java.io.File/createTempFile "tree-err" ".clj")
          path (.getAbsolutePath tmp)]
      (.deleteOnExit tmp)
      (spit path "(describe \"x\"\n  (it \"a\"\n    (it \"b\")))")
      (let [result (pc/check-file path {:tree true})]
        (should-contain "ERROR" result)
        (should-contain "(describe :line 1" result)))))

(describe "check-directory"
  (it "scans all .clj files in a directory"
    (let [results (pc/check-directory "spec/speclj_structure_check")]
      (should (pos? (count results)))
      (should (every? #(= "OK" (:result %)) results)))))

(describe "run-cli"
  (it "prints usage to stdout for --help"
    (let [out (with-out-str
                (should= 0 (pc/run-cli ["--help"])))]
      (should-contain "Usage: speclj-structure-check" out)
      (should-contain "--help" out)
      (should-contain "--tree" out)))

  (it "does not scan files when --help is present"
    (with-redefs [pc/check-file (fn [& _] (throw (ex-info "should not scan file" {})))
                  pc/check-directory (fn [& _] (throw (ex-info "should not scan dir" {})))]
      (let [out (with-out-str
                  (should= 0 (pc/run-cli ["--help" "spec"])))]
        (should-contain "Usage: speclj-structure-check" out))))

  (it "returns success for a clean file"
    (let [tmp (java.io.File/createTempFile "cli-clean" ".clj")
          path (.getAbsolutePath tmp)]
      (.deleteOnExit tmp)
      (spit path "(describe \"x\" (it \"y\"))")
      (let [out (with-out-str
                  (should= 0 (pc/run-cli [path])))]
        (should-contain (str path ": OK") out))))

  (it "returns failure for a bad file"
    (let [tmp (java.io.File/createTempFile "cli-bad" ".clj")
          path (.getAbsolutePath tmp)]
      (.deleteOnExit tmp)
      (spit path "(describe \"x\" (it \"y\" (it \"z\")))")
      (let [out (with-out-str
                  (should= 1 (pc/run-cli [path])))]
        (should-contain "ERROR" out))))

  (it "returns success for a directory of clean files"
    (let [dir (doto (java.io.File/createTempFile "cli-dir" "")
                .delete
                .mkdir)
          spec-file (java.io.File. dir "sample.clj")]
      (.deleteOnExit dir)
      (.deleteOnExit spec-file)
      (spit spec-file "(describe \"x\" (it \"y\"))")
      (let [out (with-out-str
                  (should= 0 (pc/run-cli [(.getAbsolutePath dir)])))]
        (should-contain (str (.getAbsolutePath spec-file) ": OK") out))))

  (it "prints not found for a missing path"
    (let [path "/tmp/speclj-structure-check-missing-file.clj"
          out (with-out-str
                (should= 1 (pc/run-cli [path])))]
      (should-contain (str path ": not found") out)))

  (it "returns zero only when every path succeeds"
    (with-redefs [pc/path-ok? (fn [path _] (not= path "bad"))]
      (should= 0 (#'pc/run-paths ["good" "also-good"] {}))
      (should= 1 (#'pc/run-paths ["good" "bad"] {}))))

  (it "passes tree opts through to file checks"
    (let [received (atom nil)
          tmp (java.io.File/createTempFile "cli-tree" ".clj")
          path (.getAbsolutePath tmp)]
      (.deleteOnExit tmp)
      (spit path "(describe \"x\")")
      (with-redefs [pc/check-file (fn [_ opts]
                                    (reset! received opts)
                                    "OK")]
        (with-out-str
          (should= 0 (pc/run-cli ["--tree" path]))))
      (should= {:tree true} @received))))

(run-specs)
