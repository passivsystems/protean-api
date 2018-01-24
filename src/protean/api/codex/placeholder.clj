(ns protean.api.codex.placeholder
  "Placeholder functionality, swapping codex examples, generating."
  (:refer-clojure :exclude [long int])
  (:require [clojure.string :as s]
            [clojure.set :refer [map-invert]]
            [protean.utils :as u]
            [protean.api.generation.generate :as gn]
            [protean.api.codex.document :as d]
            [protean.api.transformation.coerce :as c]))

;; =============================================================================
;; Helper functions
;; =============================================================================
; place holder has form: ${xxx}
(def ph #"\$\{([^\}]*)\}")

(defn- g-val [type structured tree]
  (if-let [regex (d/get-in-tree tree [:types type])]
    (gn/generate regex)
    (case type
      :Int (str (gn/rnd-int))
      :Long (str (gn/rnd-long))
      :Double (str (gn/rnd-double))
      :Boolean (str (gn/rnd-bool))
      :Uuid (str (gn/rnd-uuid))
      :Json (c/jsn structured)
      :MatrixParams (c/->matrix-params structured)
      (gn/generate type))))

;; =============================================================================
;; Truthiness functions
;; =============================================================================

(defn holder?
  "Does a value contain a placeholder?"
  [v]
  (cond
    (string? v) (re-seq ph v)
    (map? v)    (seq (mapcat holder? (vals v)))
    (seq? v)    (mapcat holder? v)
    (vector? v) (mapcat holder? v)
    :else       nil))

(defn map-holders [s]
  (map-invert (into {} (holder? s))))

;; =============================================================================
;; Transformation functions
;; =============================================================================

(defn- replace-loop [s func matches]
  (if (empty? matches)
    s
    (let [match (first matches)
          to-be-replaced (first match)
          term (second match)
          applied (func term)]
      (recur
        (if applied (s/replace-first s to-be-replaced applied) s)
        func
        (rest matches)))))

(defn replace-all-with
  "replace all occurrences in string of placeholder with result of applying func to the placeholder name"
  [s func]
  (replace-loop s func (holder? s)))

(declare swap)

; TODO we will probably want to control if we generate optional elements
(defn gen-struct
  "generate a structed value, where the sub-values may be recursively generated"
  [struct tree bag gen-all]
  (let [struct-with-ph (->> struct
                         (map (fn [[k v]] [k (first v)]))
                         (into {}))]
    (swap struct-with-ph tree bag :gen-all gen-all)))

(defn holder-swap
  "Swap generative values in m of placeholders."
  [m swap-fn tree]
  (cond
    (string? m) (replace-all-with m (partial swap-fn tree))
    (map? m)    (into {} (for [[k v] m] {k (holder-swap v swap-fn tree)}))
    (seq? m)    (map #(holder-swap % swap-fn tree) m)
    (vector? m) (map #(holder-swap % swap-fn tree) m)
    :else       m))

(defn swap
  "swaps all occurances of placeholders in m with values in bag
   or generated/examples from tree.
   Note vars marked as :gen false in tree will not be generated (unless optional parameter :gen-all is true)"
  [m tree bag & {:keys [gen-all]}]
  (defn fn-swap-bag [bag v] (get bag v))
  (defn fn-swap-exp [tree v] (first (d/get-in-tree tree [:vars v :examples])))
  (defn fn-swap-gen [gen-all bag tree v]
    (let [{:keys [gen type struct]} (d/get-in-tree tree [:vars v])]
      (when (and (or gen-all (not (= false gen))) type)
        (g-val type (gen-struct struct tree bag gen-all) tree))))
  (-> m
     (holder-swap fn-swap-bag bag)
     (holder-swap fn-swap-exp tree)
     (holder-swap (partial fn-swap-gen gen-all bag) tree)))

(defn- regex
 [tree n]
 (let [t (d/get-in-tree tree [:vars n :type])
       p (d/get-in-tree tree [:types t])]
   (cond
     p              p
     (= t :Int)     "^-?\\d{1,10}$"
     (= t :Long)    "^-?\\d{1,19}$"
     (= t :Double)  "^(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?$"
     (= t :Boolean) "[true|false]"
     (= t :Uuid)    "[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}"
     (= t :Json)    "^[{].*[}]"
     :else          "")))

(defn regex-pattern
 [tree v]
 (loop [s v items (re-seq ph v)]
   (if items
     (let [[[p n]] items] (recur (s/replace s p (regex tree n)) (next items)))
     (re-pattern s))))

;; =============================================================================
;; Extraction functions
;; =============================================================================

(defn- param-value
  [a b]
  (when (and a b)
    (into {}
      (for [[k _] (holder? a)]
        {(s/replace k #"[${}]" "") (second (re-find (re-pattern (s/replace k ph "(.*)")) b))}))))

(defn response-bag
  "A bag of placeholder values from the request"
  [{:keys [status]}
   {:keys [headers path-params query-params form-params tree response]}]
  (let [rsp (u/find #(= (:status %) status) (concat (:success response) (:error response)))
        rsp-holder (map-invert (into {} (holder? rsp)))
        matrix-params (into {} (filter (fn [[k _]] (s/starts-with? k ";")) path-params))]
    (merge
      (into {} (for [[k [v]] (d/req-hdrs tree)]
        (param-value v (get headers (s/lower-case k)))))
      (into {} (for [[k v] (into {} path-params)]
        (param-value (get rsp-holder k) v)))
      (into {} (for [[k [v]] (d/qps tree)]
        (param-value v (get query-params k))))
      (into {} (for [[k v] matrix-params
                     :let [[a b] (s/split v #"=")]]
        (param-value (get rsp-holder (s/replace a #";" "")) b)))
      (into {} (for [[k [v]] (d/fps tree)]
        (param-value v (get form-params k)))))))


(defn- diff [s1 s2]
  (cond
    (and (nil? (first s1)) (nil? (first s2))) []
    (= (first s1) (first s2)) (diff (rest s1) (rest s2))
    :else [s1 s2]))

(defn- diff-str [s1 s2]
  (into [] (map s/join (diff (char-array (str s1)) (char-array (str s2))))))

; note currently only works until first mismatch.
; Which only works if our placeholder is the only placeholder, and is at the end of the string.
; e.g. abc${def} - ok
;      abc${def}ghi - not ok
(defn read-from [template a-ph s]
  (let [[left right] (diff-str template s)
        diff-match (if left (re-matches ph left))]
    (when (= (second diff-match) a-ph)
      right)))
