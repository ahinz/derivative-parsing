(ns pcomb.core
  (:require [clojure.spec :as s]))

(defprotocol Parser
  (-parse-null [_])
  (-is-nullable? [_])
  (-is-empty? [_])
  (-derivep [_ c]))

(s/def ::strict-parser #(satisfies? Parser %))
(s/def ::lazy-parser delay?)
(s/def ::parser (s/or :strict ::strict-parser
                      :lazy ::lazy-parser))
(s/def ::token ::s/any)

(s/fdef parse-null
        :args (s/cat :parser ::parser)
        :ret (s/coll-of ::token))

(defn parse-null [parser]
  (-parse-null parser))

(s/fdef is-nullable?
        :args (s/cat :parser ::parser)
        :ret boolean?)

(defn is-nullable? [parser]
  (-is-nullable? parser))

(s/fdef is-empty?
        :args (s/cat :parser ::parser)
        :ret boolean?)

(defn is-empty? [parser]
  (-is-empty? parser))

(s/fdef derivep
        :args (s/cat :parser ::parser
                     :char char?)
        :ret ::parser)

(defn derivep [parser char]
  (-derivep parser char))

(s/fdef parse-full
        :args (s/cat :parser ::parser
                     :input-stream (s/or :char-col (s/coll-of char?)
                                         :string string?))
        :ret (s/coll-of ::s/any))

(defn parse-full [parser input-stream]
  (if (empty? input-stream)
    (parse-null parser)
    (recur (derivep parser (first input-stream))
           (rest input-stream))))

(def empty-language
  "The empty language accepts no strings and returns an empty null parse"
  (reify Parser
    (-is-nullable? [_] false)
    (-is-empty? [_] true)
    (-derivep [_ chr]
      empty-language)
    (-parse-null [this]
      nil)))

(defn null-string
  "The null string combinator accepts only the empty string, return
   the value of `outp` for the null result"
  [outp]
  (reify Parser
    (-is-nullable? [_] true)
    (-is-empty? [_] false)
    (-derivep [_ chr]
      empty-language)
    (-parse-null [_]
      outp)))

(defn char-parser
  "Parses a single character"
  [c]
  (reify Parser
    (-is-nullable? [_] false)
    (-is-empty? [_] false)
    (-derivep [_ chr]
      (if (= chr c)
        (null-string #{(str chr)})
        empty-language))
    (-parse-null [this]
      nil)))

;; Clojure doesn't quite directly support lazy arguments
;; for normal functions but we need lazy semantics for reduction,
;; alternation (union) and concatenation.
;;
;; To achieve the required laziness transparently the operations
;; are written as macros, wrapping arguments in `delay` objects
;; and passing them to the *'d functions
;;

(defmacro reduction-parser
  "Given a combinator `parser`, apply the given function `f` to
   the parsed output"
  [parser f]
  `(reduction-parser* (delay ~parser) ~f))

(defn reduction-parser* [parser f]
  (reify Parser
    (-is-nullable? [_] (is-nullable? @parser))
    (-is-empty? [_] (is-empty? @parser))
    (-derivep [_ chr]
      (reduction-parser (derivep @parser chr) f))
    (-parse-null [this]
      (map f (parse-null @parser)))))

(defmacro alt-parser
  "Given a sequence of parsers return a combinator that
   matches any of them"
  [parsers]
  `(alt-parser* (delay ~parsers)))

(defn alt-parser* [parsers]
  (reify Parser
    (-is-nullable? [_] (not-every? false? (map is-nullable? @parsers)))
    (-is-empty? [_] false)  ;; This is only used for a shortcut, okay to not respond (I think)
    (-derivep [_ chr]
      ;; Remove all null parsers
      (let [non-empty-parsers (remove is-empty? @parsers)]
        (cond
          (empty? non-empty-parsers)
          empty-language

          (= (count non-empty-parsers) 1)
          (derivep (first non-empty-parsers) chr)

          :else
          (alt-parser (map (fn [parser]
                             (derivep parser chr)) non-empty-parsers)))
        (alt-parser (map (fn [parser]
                             (derivep parser chr)) @parsers))))
    (-parse-null [this]
      (mapcat parse-null @parsers))))

(defmacro and-parser
  "Given two parsers return a parser that matches
   `parser1` followed by `parser2`"
  [parser1 parser2]
  `(and-parser* (delay ~parser1) (delay ~parser2)))

(defn and-parser* [parser1 parser2]
  (reify Parser
    (-is-nullable? [_] (and (is-nullable? @parser1)
                            (is-nullable? @parser2)))
    (-is-empty? [_] (or (is-empty? @parser1)
                        (is-empty? @parser2)))
    (-derivep [_ chr]
      (if (is-nullable? @parser1)
        (alt-parser
         [(and-parser (derivep @parser1 chr) @parser2)
          (and-parser (null-string (parse-null @parser1))
                      (derivep @parser2 chr))])
        (and-parser (derivep @parser1 chr) @parser2)))
    (-parse-null [this]
      (mapcat (fn [t1]
                (map (partial vector t1) (parse-null @parser2)))
              (parse-null @parser1)))))

(defn left-joining-and-parser [p1 p2]
  (reduction-parser (and-parser p1 p2) (fn [[xs x]]
                                         (conj xs x))))

(defn right-joining-and-parser [p1 p2]
  (reduction-parser (and-parser p1 p2) (fn [[x xs :as m]]
                                         (conj xs x))))

(defmacro concat-parser
  "Given a seq of parsers `p1, p2, ... pn` return a parser
   that matches `p1` followed by `p2` ... `pn`"
  [parsers]
  `(concat-parser* (delay ~parsers)))

(defn concat-parser* [lazy-parsers]
  (let [[parser & parsers] @lazy-parsers]
   (if (empty? parsers)
     parser
     (reduce left-joining-and-parser
             (and-parser parser (first parsers))
             (rest parsers)))))


(defn string-parser
  "Return a parser matching the given string"
  [s]
  (if (empty? s)
    (null-string #{""})
    (reduction-parser (concat-parser (map char-parser s)) (partial apply str))))

(defn char-set-parser
  "Return a parser that matches any character in `cs`"
  [cs]
  (alt-parser (map (fn [c] (char-parser c)) cs)))

(defn char-range-parser [& ranges]
  (char-set-parser
   (mapcat (fn [[r1 r2]]
             (map char (range (int r1) (inc (int r2)))))
           (partition 2 ranges))))

(defn kleene-parser
  "Return a parser that matches `parser` any number of times"
  [parser]
  (alt-parser [(null-string #{'()})
               (right-joining-and-parser parser (kleene-parser parser))]))

;; Implemented directly, performance improvement?
(defn kleene-parser*
  "Return a parser that matches `parser` any number of times"
  [parser]
  (reify Parser
    (-is-nullable? [_] true)
    (-is-empty? [_] (is-empty? parser))
    (-derivep [_ chr]
      (reduction-parser
       (and-parser (derivep parser chr) (kleene-parser* parser))
       (fn [[a b]] (conj b a))))
    (-parse-null [this]
      #{nil})))
