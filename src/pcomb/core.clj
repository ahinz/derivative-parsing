(ns pcomb.core)

(defprotocol Parser
  (parse-null [_])
  (is-empty? [_])
  (derive [_ c]))

(defn parse-full [parser input-stream]
  (if (empty? input-stream)
    (parse-null parser)
    (recur (derive parser (first input-stream))
           (rest input-stream))))

(def empty-language
  (reify Parser
    (is-empty? [_] true)
    (derive [_ chr]
      empty-language)
    (parse-null [this]
      nil)))

(defn null-string [outp]
  (reify Parser
    (is-empty? [_] true)
    (derive [_ chr]
      empty-language)
    (parse-null [_]
      outp)))

(defn reduction-parser [t f]
  (reify Parser
    (is-empty? [_] (is-empty? t))
    (derive [_ chr]
      (reduction-parser (derive t chr) f))
    (parse-null [this]
      (map f (parse-null t)))))

(defn char-parser [c]
  (reify Parser
    (is-empty? [_] false)
    (derive [_ chr]
      (if (= chr c)
        (null-string #{(str chr)})
        empty-language))
    (parse-null [this]
      nil)))

(defn alt-parser [parser1 parser2]
  (reify Parser
    (is-empty? [_] (or (is-empty? parser1)
                       (is-empty? parser2)))
    (derive [_ chr]
      (alt-parser (derive parser1 chr)
                  (derive parser2 chr)))
    (parse-null [this]
      (concat (parse-null parser1)
              (parse-null parser2)))))

(defn alt-parser* [& parsers]
  (reify Parser
    (is-empty? [_] (not-every? false? (map is-empty?) parsers))
    (derive [_ chr]
      (apply alt-parser* (map #(derive % chr) parsers)))
    (parse-null [this]
      (mapcat parse-null parsers))))


(defn and-parser [parser1 parser2]
  (reify Parser
    (is-empty? [_] (or (is-empty? parser1)
                       (is-empty? parser2)))
    (derive [_ chr]
      (if (is-empty? parser1)
        (alt-parser
         (and-parser (derive parser1 chr) parser2)
         (and-parser (null-string (parse-null parser1))
                     (derive parser2 chr)))
        (and-parser (derive parser1 chr) parser2)))
    (parse-null [this]
      (mapcat (fn [t1]
                (map (partial vector t1) (parse-null parser2)))
              (parse-null parser1)))))

(defn left-joining-and-parser [p1 p2]
  (reduction-parser (and-parser p1 p2) (fn [[xs x]]
                                         (conj xs x))))

(defn and-parser* [& parsers]
  (reduce left-joining-and-parser
          (and-parser (first parsers) (second parsers))
          (drop 2 parsers)))

(defn string-parser [s]
  (reduction-parser (apply and-parser* (map char-parser s)) (partial apply str)))

(defn char-set-parser [cs]
  (apply alt-parser* (map char-parser cs)))
