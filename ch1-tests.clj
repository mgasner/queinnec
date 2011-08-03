(load-file "/Users/maxgasner/Desktop/lisp-in-small-pieces/ch1.clj")

(defmacro unit-test
  [exp env res]
  `(= (evaluate ~exp ~env) ~res))

(unit-test '(if true true false) (hash-map) true)
(unit-test '(if true false true) (hash-map) false)
(unit-test '(if false false true) (hash-map) true)
(unit-test '(if false true false) (hash-map) false)