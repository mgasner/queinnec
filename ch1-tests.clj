(load-file "/Users/maxgasner/Desktop/lisp-in-small-pieces/ch1.clj")

(defmacro unit-test
  [exp env res]
  `(= (evaluate ~exp ~env) ~res))

(and (unit-test '(if true true false) (hash-map) true)
(unit-test '(if true false true) (hash-map) false)
(unit-test '(if false false true) (hash-map) true)
(unit-test '(if false true false) (hash-map) false)
(unit-test 3 (hash-map) 3)
(unit-test "humbug" (hash-map) "humbug")
(unit-test false (hash-map) false)
(unit-test [1 2 3] (hash-map) [1 2 3])
(unit-test 'h (ref (assoc (hash-map) (keyword 'h) 3)) 3)
(unit-test '(+ 2 1) env-global 3)
(unit-test '(begin (set! foo 3) (if (< foo 5) foo 0)) env-global 3)
(unit-test '(begin (set! foo 5) foo) env-global 5)
(unit-test '(quote foo) env-global 'foo)
(unit-test '((lambda (x y z) (+ y z)) 1 2 3) env-global 5)
(unit-test '(begin (set! foo ((lambda (x) (+ x 1)) 3)) (+ foo 5)) env-global 9)
(unit-test '((lambda (x) (+ x (if (< x 5) 2 1))) 8) env-global 9)
(unit-test '(((lambda (x y) (lambda (msg) (if (= msg 'car) x (if (= msg 'cdr) y nil)))) 3 2) 'car) env-global 3)
(unit-test '(((lambda (x y) (lambda (msg) (if (= msg 'car) x (if (= msg 'cdr) y nil)))) 3 2) 'humbug) env-global 'nil))