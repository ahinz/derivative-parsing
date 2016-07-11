# Parsing with Derivatives

This is an implementation of parsing using language derivatives that can
be found in [Yacc is dead](http://arxiv.org/abs/1010.5023).

Verification is done in `pcomb.core-test` using some regular testing but
mostly using `clojure.test.check`.

There are a few "gotchas" relating to the library:
* Performance - this is mostly a proof of concept and right now has a
pretty terrible runtime
* Laziness - Derivative based parsing requires certain combinators to be
  lazy in their arguments, which is provided via macros in clojure.

## Usage

In `pcomb.core` you can play around with the basic combinators:

```clojure
pcomb.core> (parse-full (alt-parser [(char-parser \a) (char-parser \b)]) "a")
("a")

pcomb.core> (parse-full (and-parser (char-parser \a) (kleene-parser (char-parser \b))) "abbbbb")
(["a" ("b" "b" "b" "b" "b")])
```

Syntax helpers are provided to make this a little less verbose:

```clojure
pcomb.syntax> (p/parse-full (| (c \a) (c \b)) "a")
("a")

pcomb.syntax> (p/parse-full (++ (c \a) (* (c \b))) "abbbbb")
(["a" ("b" "b" "b" "b" "b")])
```

There is also a small sexp-based parser in `pcomb.sexp`:

```clojure
pcomb.sexp> (pprint (parse "(+ 1 (* a 3))"))
{:pcomb.sexp/tag :pcomb.sexp/sexp,
 :pcomb.sexp/value
 ({:pcomb.sexp/tag :pcomb.sexp/ident, :pcomb.sexp/value "+"}
  {:pcomb.sexp/tag :pcomb.sexp/number, :pcomb.sexp/value 1}
  {:pcomb.sexp/tag :pcomb.sexp/sexp,
   :pcomb.sexp/value
   ({:pcomb.sexp/tag :pcomb.sexp/ident, :pcomb.sexp/value "*"}
    {:pcomb.sexp/tag :pcomb.sexp/ident, :pcomb.sexp/value "a"}
    {:pcomb.sexp/tag :pcomb.sexp/number, :pcomb.sexp/value 3})})}

pcomb.sexp> (eval-exp (parse "(+ 1 9 (* 2 3) (/ 12 4))") default-scope)
19
```

## License

Copyright © 2016 Adam Hinz

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
