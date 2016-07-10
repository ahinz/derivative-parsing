(ns pcomb.syntax
  (:require [pcomb.core :as p])
  (:refer-clojure :exclude [+ * ->]))

(def * p/kleene-parser)

(defn + [parser]
  (p/right-joining-and-parser parser (p/kleene-parser parser)))

(defn c [chr]
  (p/char-parser chr))

(defmacro ++ [& parsers]
  `(p/concat-parser ~(into [] parsers)))

(defmacro | [& parsers]
  `(p/alt-parser ~(into [] parsers)))

(defmacro -> [parser f]
  `(p/reduction-parser ~parser ~f))
