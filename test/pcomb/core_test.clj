(ns pcomb.core-test
  (:require [clojure.test :refer :all]
            [pcomb.core :refer :all]
            [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(stest/instrument)

(def itrs 10)

;;
;; Literal parsers
;;

(deftest empty-language-test
  (testing "is-empty"
    (is (= true (is-empty? empty-language))))

  (testing "derivation is empty"
    (is (= true (is-empty? (derivep empty-language \c))))))

(defn derive-recur [parser chars]
  (reduce derivep parser chars))

(defspec empty-language-is-empty-for-all-chars
  itrs
  (prop/for-all [c (gen/vector gen/char)]
    (is (= true (is-empty? (derive-recur empty-language c))))))

(defspec empty-language-rejects-all-inputs
  itrs
  (prop/for-all [s gen/string]
    (is (empty? (parse-full empty-language s)))))

(defspec null-string-is-empty-for-all-chars
  itrs
  (prop/for-all [c (gen/vector gen/char)]
    (is (= true (is-empty? (derive-recur (null-string []) c))))))

(defspec null-string-language-rejects-all-inputs-non-empty
  itrs
  (prop/for-all [s (gen/not-empty gen/string)
                 ret (gen/vector gen/char)]
    (is (empty? (parse-full (null-string ret) s)))))

(deftest null-string-language-parsed-empty-string
  (testing "empty string"
    (is (= (set (parse-full (null-string #{1 2 3}) "")) #{1 2 3}))
    (is (empty? (parse-full (null-string #{1 2 3}) "f")))))

(defspec char-parser-parses-chars
  itrs
  (prop/for-all [c gen/char]
    (= (set (parse-full (char-parser c) [c])) #{(str c)})))

(defspec char-parser-derives-to-char-when-matching
  itrs
  (prop/for-all [c gen/char]
    (let [parser (derivep (char-parser c) c)]
      (and (is-empty? parser)
           (= (parse-null parser) #{(str c)})))))

(defspec char-parser-derives-empty-when-not-matching
  itrs
  (prop/for-all [chars (gen/set gen/char {:num-elements 2})]
    (let [[c1 c2] (vec chars)
          parser (derivep (char-parser c1) c2)]
      (empty? (parse-null parser)))))

;;
;; Core Combinators
;;

(defspec reduction-parser-applies-fn
  itrs
  (prop/for-all [chars (gen/not-empty (gen/vector gen/char))]
    (let [f (into {} (map vector (map str chars) (map str (shuffle chars))))
          c (rand-nth chars)
          parser (char-parser c)
          parser* (reduction-parser parser f)]
      (= (set (parse-full parser* [c])) #{(f (str c))}))))

(defspec alt-parser-matches-branches
  itrs
  (prop/for-all [chars (gen/not-empty (gen/vector gen/char))]
    (let [parser (apply alt-parser (map char-parser chars))
          parses? (fn [c]
                    (= (set (parse-full parser [c])) #{(str c)}))]
      (every? true? (map parses? chars)))))

(defspec and-parser-requires-both
  itrs
  (prop/for-all [chars (gen/set gen/char {:num-elements 2})]
    (let [[c1 c2] (vec chars)
          parser (and-parser (char-parser c1) (char-parser c2))]
      (and (empty? (parse-full parser [c1]))
           (empty? (parse-full parser [c2]))
           (= (set (parse-full parser [c1 c2])) #{[(str c1) (str c2)]})))))

(defspec string-parser-test
  itrs
  (prop/for-all [s gen/string]
    (= (set (parse-full (string-parser s) s)) #{s})))
