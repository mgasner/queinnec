;(ns lisp-ch1)
(declare evaluate eprogn evlis env-global env-init lookup update! extend-env make-function invoke third fourth empty-begin atom? boolean?)

(defn evaluate
  [exp env]
  (if (atom? exp)
      (cond (symbol? exp)
              (lookup exp env)
            (or (number? exp) (string? exp) (char? exp)
                (boolean? exp) (vector? exp))
              exp
            :else (throw (Exception. (str "Cannot evaluate " exp))))
      (let [term (first exp)]
        (cond (= term (symbol "quote"))
                (second exp)
              (= term (symbol "if"))
                (if (evaluate (second exp) env) 
                    (evaluate (third exp) env)
                    (evaluate (fourth exp) env))
              (= term (symbol "begin"))
                (eprogn (rest exp) env)
              (= term (symbol "set!"))
                (update! (second exp) env (evaluate (third exp) env))
              (= term (symbol "lambda"))
                (make-function (second exp) (third exp) env)
              :else
                (invoke (evaluate (first exp) env) (evlis (rest exp) env))))))

(defn eprogn
  [exps env]
  (if (list? exps)
    (if (list? (rest exps))
        (do (evaluate (first exps) env)
            (recur (rest exps) env))
        (evaluate (first exps) env))
    empty-begin))

(defn evlis
  [exps env]
  (if (list? exps)
      (let [argument1 (evaluate (first exps) env)]
        (cons argument1 (evlis (rest exps) env)))
      '()))
  
; representing environments as maps, not a-lists
(def env-init (ref (hash-map)))

(defn lookup
  [id env]
  (get (deref env) (keyword id) (throw (Exception. (str "No such binding: " id)))))

(defn update!
  [id env value]
  (if (contains? (deref env) (keyword id))
      (dosync (ref-set env (assoc (deref env) (keyword id) value)))
      (throw (Exception. (str "No such binding: " id)))))
      
(defn extend-env
  [env variables values]
  (cond (list? variables)
          (if (list? values)
              (recur (ref (assoc (deref env) (keyword (first variables)) (first values))) (rest variables) (rest values))
              (throw (Exception. "Too few values")))
        (empty? variables)
          (if (empty? values)
              env
              (throw (Exception. "Too many values")))
        (symbol? variables)
          (dosync
            (ref-set env (assoc (deref env) (keyword variables) values))
            env)))

(defn make-function
  [variables body env]
  (fn
    [values]
    (eprogn body (extend env variables values))))

(defn invoke
  [fn args]
  (if (fn? fn)
      (fn args)
      (throw (Exception. (str "Not a function" fn)))))

(defn third
  [l]
  (first (rest (rest l))))
  
(defn fourth
  [l]
  (first (rest (rest (rest l)))))
  
(defn atom?
  [x]
  (not (list? x)))

(defn boolean?
  [x]
  (or (= x true) (= x false)))

(def empty-begin 813)

(def env-global env-init)

(defmacro definitialname
  [name] `(dosync (ref-set env-global (assoc (deref env-global) (keyword ~name) nil))))

(defmacro definitial
  [name value] `(dosync (ref-set env-global (assoc (deref env-global) (keyword ~name) ~value))))

(defmacro defprimitive
  [name value arity]
  `(definitial ~name (fn [values#] (if (= ~arity (count values#))
                                  (apply ~value values#)
                                  (throw (Exception. ("Incorrect arity")))))))

(definitial 't true)
(definitial 'f false)
(definitial 'nil nil)
(definitialname 'foo)
(definitialname 'bar)
(definitialname 'fib)
(definitialname 'fact)
(defprimitive 'cons conj 2)
(defprimitive 'car first 1)
(defprimitive '+ + 2)
(defprimitive '= = 2)
(defprimitive '< < 2)

(defn chapter1-scheme
  []
  (defn toplevel
    []
    (printf (evaluate (read-line) env-global))
    (recur))
  (toplevel))