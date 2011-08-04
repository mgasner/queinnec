;(ns lisp-ch1)
(declare evaluate eprogn evlis env-global env-init lookup update! extend-env make-function invoke third fourth empty-begin atom? boolean?)

(defn evaluate
  [exp env]
  ;(prn exp)
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
                (make-function (second exp) (rest (rest exp)) env)
              :else
                (invoke (evaluate (first exp) env) (evlis (rest exp) env))))))

(defn eprogn
  [exps env]
  (if (empty? exps)
    empty-begin
    (if (empty? (rest exps))
        (evaluate (first exps) env)
        (do (evaluate (first exps) env)
            (recur (rest exps) env)))))
            
(defn evlis
  [exps env]
  (if (empty? exps)
      '()
      (cons (evaluate (first exps) env) (evlis (rest exps) env))))
      
    
; representing environments as maps, not a-lists
(def env-init (ref (hash-map)))

(defn lookup
  [id env]
  (if (contains? (deref env) (keyword id))
      (get (deref env) (keyword id))
      (throw (Exception. (str "No such binding: " id)))))

(defn update!
  [id env value]
  (if (contains? (deref env) (keyword id))
      (dosync (ref-set env (assoc (deref env) (keyword id) value)))
      (throw (Exception. (str "No such binding: " id)))))
      
(defn extend-env
  [env variables values]
  (cond (empty? variables)
          (if (empty? values)
              env
              (throw (Exception. "Too many values")))
        (seq? variables)
          (if (seq? values)
              (recur (ref (assoc (deref env) (keyword (first variables)) (first values))) (rest variables) (rest values))
              (throw (Exception. "EXTEND-ENV: Too few values")))
        (symbol? variables)
          (dosync
            (ref-set env (assoc (deref env) (keyword variables) values))
            env)))

(defn make-function
  [variables body env]
  (fn
    [vals]
    (eprogn body (extend-env env variables vals))))

(defn invoke
  [fn args]
  ;(prn fn args)
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
  (not (seq? x)))

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
(defprimitive 'cons cons 2)
(defprimitive 'car first 1)
(defprimitive '+ + 2)
(defprimitive '= = 2)
(defprimitive '< < 2)
