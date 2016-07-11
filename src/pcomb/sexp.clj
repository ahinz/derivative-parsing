(ns pcomb.sexp
  (:require [pcomb.core :as p]
            [pcomb.syntax :as s]))

(defn tag [t v]
  {::tag t
   ::value v})

(def numbers (p/char-set-parser #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}))
(def integer-parser (s/-> (s/+ numbers) (fn [i] (Integer/parseInt (apply str i)))))
(def double-parser (s/-> (s/++ (s/+ numbers) (p/char-parser \.) (s/+ numbers))
                         (fn [[a _ b]]
                           (Double/parseDouble (format "%s.%s" (apply str a) (apply str b))))))

(def alpha (p/char-range-parser \a \z \A \Z))
(def symbols (p/char-set-parser #{\+ \- \* \\}))

(def ident-parser (s/-> (s/| symbols
                             (s/++ alpha
                                   (s/+ (s/| alpha numbers))))
                        (fn [[f rst]]
                          (tag ::ident (apply str f rst)))))

(def number-parser (s/-> (s/| integer-parser double-parser)
                         (partial tag ::number
                                  )))

(def whitespace-chars (p/char-set-parser #{\space \newline \tab}))
(def whitespace-parser (s/-> (s/+ whitespace-chars)
                             (fn [_] (tag ::whitespace nil))))

(declare sexp-parser)

(def expression-parser (s/| number-parser ident-parser sexp-parser))

(def ignore-tag? (comp #{nil ::whitespace} ::tag))

(def expression-list-parser* (s/* (s/-> (s/++ whitespace-parser expression-parser)
                                        (fn [[_ e]] e))))

(def expression-list-parser (s/-> (s/++ expression-parser expression-list-parser*)
                                  (fn [[e es]]
                                    (remove ignore-tag? (concat [e] es)))))

(def sexp-parser (s/-> (s/++ (s/c \() expression-list-parser (s/c \)))
                       (fn [[_ s _]] (tag ::sexp s))))

(defn parse [s]
  (first (p/parse-full expression-parser s)))

(def default-scope {"+" + "*" * "-" - "/" /})

(defmulti eval-exp ::tag)
(defmethod eval-exp ::ident [{ident ::value} scope]
  (if-let [f (get scope ident)]
    f
    (throw (RuntimeException. (format "Identifier `%s' not in scope" ident)))))

(defmethod eval-exp ::number [{n ::value} _]
  n)

(defmethod eval-exp ::sexp [{values ::value} scope]
  (let [[f & args] (map #(eval-exp % scope) values)]
    (apply f args)))
