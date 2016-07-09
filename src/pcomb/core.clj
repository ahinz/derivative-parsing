(ns pcomb.core
  (:require [clojure.spec :as s]))

(defprotocol Parser
  (-parse-null [_])
  (-is-empty? [_])
  (-derivep [_ c]))

(s/def ::strict-parser #(satisfies? Parser %))
(s/def ::lazy-parser delay?)
(s/def ::parser ::strict-parser)
(s/def ::token ::s/any)

(s/fdef parse-null
        :args (s/cat :parser ::parser)
        :ret (s/coll-of ::token))

(defn parse-null [parser]
  (-parse-null parser))

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
  (reify Parser
    (-is-empty? [_] true)
    (-derivep [_ chr]
      empty-language)
    (-parse-null [this]
      nil)))

(defn null-string [outp]
  (reify Parser
    (-is-empty? [_] true)
    (-derivep [_ chr]
      empty-language)
    (-parse-null [_]
      outp)))

(defn char-parser [c]
  (reify Parser
    (-is-empty? [_] false)
    (-derivep [_ chr]
      (if (= chr c)
        (null-string #{(str chr)})
        empty-language))
    (-parse-null [this]
      nil)))

(defn reduction-parser [t f]
  (reify Parser
    (-is-empty? [_] (is-empty? t))
    (-derivep [_ chr]
      (reduction-parser (derivep t chr) f))
    (-parse-null [this]
      (map f (parse-null t)))))

(defn alt-parser [& parsers]
  (reify Parser
    (-is-empty? [_] (not-every? false? (map is-empty? parsers)))
    (-derivep [_ chr]
      (apply alt-parser (map #(derivep % chr) parsers)))
    (-parse-null [this]
      (mapcat parse-null parsers))))


(defn and-parser [parser1 parser2]
  (reify Parser
    (-is-empty? [_] (and (is-empty? parser1)
                         (is-empty? parser2)))
    (-derivep [_ chr]
      (if (is-empty? parser1)
        (alt-parser
         (and-parser (derivep parser1 chr) parser2)
         (and-parser (null-string (parse-null parser1))
                     (derivep parser2 chr)))
        (and-parser (derivep parser1 chr) parser2)))
    (-parse-null [this]
      (mapcat (fn [t1]
                (map (partial vector t1) (parse-null parser2)))
              (parse-null parser1)))))

(defn left-joining-and-parser [p1 p2]
  (reduction-parser (and-parser p1 p2) (fn [[xs x :as m]]
                                         (conj xs x))))

(defn concat-parser [parser & parsers]
  (if (empty? parsers)
    parser
    (reduce left-joining-and-parser
            (and-parser parser (first parsers))
            (rest parsers))))

(defn string-parser [s]
  (if (empty? s)
    (null-string #{""})
    (reduction-parser (apply concat-parser (map char-parser s)) (partial apply str))))

(defn char-set-parser [cs]
  (apply alt-parser (map char-parser cs)))
