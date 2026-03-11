;; mutation-tested: 2026-03-11
(ns speclj-structure-check.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private speclj-keywords
  #{"describe" "context" "it" "before" "before-all"
    "after" "with-stubs" "with" "around"})

(defn- handle-comment-char [state c line]
  (if (= c \newline)
    (assoc state :mode :normal :line (inc line))
    state))

(defn- handle-quoted-char [state c]
  (cond
    (= c \\) (assoc state :escape true)
    (= c \") (assoc state :mode :normal)
    (= c \newline) (update state :line inc)
    :else state))

(defn- handle-normal-char [state c next-c]
  (cond
    (= c \;) (assoc state :mode :comment)
    (= c \\) (assoc state :escape true)
    (= c \") (assoc state :mode :string)
    (and (= c \#) (= next-c \")) (assoc state :mode :regex :skip true)
    (= c \newline) (update state :line inc)
    (= c \() (update state :depth inc)
    (= c \)) (update state :depth dec)
    :else state))

(defn- process-char [state c next-c]
  (let [{:keys [mode depth line escape skip]} state]
    (cond
      skip
      (assoc state :skip false)

      escape
      (assoc state :escape false)

      (= mode :comment)
      (handle-comment-char state c line)

      (= mode :string)
      (handle-quoted-char state c)

      (= mode :regex)
      (handle-quoted-char state c)

      ;; :normal mode
      :else
      (handle-normal-char state c next-c))))

(def ^:private token-delimiters #{\space \newline \tab \( \) \"})

(defn- extract-token [chars i]
  (let [n (count chars)
        start (inc i)]
    (when-let [_ (get chars start)]
      (loop [end start]
        (cond
          (= end n) (apply str (subvec chars start end))
          (token-delimiters (nth chars end))
          (when (not= end start)
            (apply str (subvec chars start end)))
          :else
          (recur (inc end)))))))

(defn- validate-nesting [form line form-stack]
  (when-let [parent (peek form-stack)]
    (let [parent-form (:form parent)]
      (cond
        (= parent-form "it")
        (str "ERROR line " line ": (" form ") inside (it) at line " (:line parent))

        (and (= parent-form "describe") (= form "describe"))
        (str "ERROR line " line ": (describe) inside (describe) at line " (:line parent))

        (and (= parent-form "context") (= form "describe"))
        (str "ERROR line " line ": (describe) inside (context) at line " (:line parent))))))

(defn- pop-form [form-stack]
  (let [completed (peek form-stack)
        stack (pop form-stack)
        entry (dissoc completed :depth)]
    (if (empty? stack)
      {:stack [] :form entry}
      (let [parent (peek stack)
            children (conj (or (:children parent) []) entry)
            parent (assoc parent :children children)]
        {:stack (conj (pop stack) parent) :form nil}))))

(defn- maybe-push-form [state chars i old-depth new-depth]
  (if (and (= \( (nth chars i))
           (> new-depth old-depth))
    (let [token (extract-token chars i)]
      (if (and token (speclj-keywords token))
        (let [error (validate-nesting token (:line state) (:form-stack state))]
          (-> (if error
                (update state :errors conj error)
                state)
              (update :form-stack conj
                      {:form token :line (:line state) :depth old-depth})))
        state))
    state))

(defn- completed-form? [state c old-depth new-depth old-mode]
  (and (= old-mode :normal)
       (= c \))
       (< new-depth old-depth)
       (seq (:form-stack state))
       (= new-depth (:depth (peek (:form-stack state))))))

(defn- maybe-pop-form [state c old-depth new-depth old-mode]
  (if (completed-form? state c old-depth new-depth old-mode)
    (let [{:keys [stack form]} (pop-form (:form-stack state))]
      (if form
        (-> state
            (assoc :form-stack stack)
            (update :forms conj form))
        (assoc state :form-stack stack)))
    state))

(defn- step-scan [chars n state i]
  (let [c (nth chars i)
        next-c (when (< (inc i) n)
                 (nth chars (inc i)))
        old-depth (:depth state)
        old-mode (:mode state)
        next-state (process-char state c next-c)
        new-depth (:depth next-state)]
    (if (and (= old-mode :normal) (= c \())
      (maybe-push-form next-state chars i old-depth new-depth)
      (maybe-pop-form next-state c old-depth new-depth old-mode))))

(defn- add-unclosed-errors [result]
  (let [eof-line (:line result)
        unclosed-errors (mapv (fn [entry]
                                (str "ERROR line " eof-line
                                     ": unclosed (" (:form entry)
                                     ") from line " (:line entry)))
                              (:form-stack result))]
    (update result :errors into unclosed-errors)))

(defn scan [text]
  (let [chars (vec text)
        n (count chars)
        init {:mode :normal :depth 0 :line 1 :escape false
              :skip false :errors [] :form-stack [] :forms []}
        result (reduce #(step-scan chars n %1 %2) init (range n))
        result (add-unclosed-errors result)]
    (select-keys result [:errors :depth :forms])))

(defn- format-node [node indent]
  (let [prefix (apply str (repeat indent \space))
        header (str prefix "(" (:form node) " :line " (:line node) ")")
        children (:children node)]
    (if (empty? children)
      header
      (let [child-strs (map #(format-node % (+ indent 2)) children)
            open (str prefix "(" (:form node) " :line " (:line node))
            close ")"]
        (str open "\n" (str/join "\n" child-strs) close)))))

(defn format-tree [forms]
  (str/join "\n" (map #(format-node % 0) forms)))

(defn check-file
  ([path] (check-file path {}))
  ([path opts]
   (let [text (slurp path)
         result (scan text)
         errors (:errors result)
         base (if (empty? errors)
                "OK"
                (str/join "\n" errors))]
     (if (:tree opts)
       (let [tree (format-tree (:forms result))]
         (if (empty? tree) base (str base "\n" tree)))
       base))))

(defn- find-clj-files [dir]
  (let [f (io/file dir)]
    (when (.isDirectory f)
      (->> (file-seq f)
           (filter #(and (.isFile %) (str/ends-with? (.getName %) ".clj")))
           (sort-by #(.getAbsolutePath %))))))

(defn check-directory
  ([path] (check-directory path {}))
  ([path opts]
   (mapv (fn [f]
           {:file (.getAbsolutePath f)
            :result (check-file (.getAbsolutePath f) opts)})
         (find-clj-files path))))

(def usage
  (str/join
   "\n"
   ["Usage: speclj-structure-check [--tree] [--help] <path>..."
    ""
    "Options:"
    "  --tree  Print the Speclj form tree after the result"
    "  --help  Print this help message"]))

(defn- success-exit []
  (count []))

(defn- failure-exit []
  (count [:failed]))

(defn- cli-flags [args]
  (set (filter #(str/starts-with? % "--") args)))

(defn- cli-paths [args]
  (remove #(str/starts-with? % "--") args))

(defn- tree-opts [flags]
  (cond-> {} (flags "--tree") (assoc :tree true)))

(defn- result-ok? [result]
  (str/starts-with? result "OK"))

(defn- print-file-result [path opts]
  (let [result (check-file path opts)]
    (println (str path ": " result))
    (result-ok? result)))

(defn- print-directory-results [path opts]
  (let [results (check-directory path opts)]
    (doseq [{:keys [file result]} results]
      (println (str file ": " result)))
    (every? (comp result-ok? :result) results)))

(defn- print-missing-path [path]
  (println (str path ": not found"))
  nil)

(defn- path-ok? [path opts]
  (let [f (io/file path)]
    (cond
      (.isFile f) (print-file-result path opts)
      (.isDirectory f) (print-directory-results path opts)
      :else (print-missing-path path))))

(defn- run-paths [paths opts]
  (if (every? #(path-ok? % opts) paths)
    (success-exit)
    (failure-exit)))

(defn run-cli [args]
  (let [flags (cli-flags args)
        paths (cli-paths args)]
    (if (flags "--help")
      (do
        (println usage)
        (success-exit))
      (run-paths paths (tree-opts flags)))))

(defn -main [& args]
  (System/exit (run-cli args)))
