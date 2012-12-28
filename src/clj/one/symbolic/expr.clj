(ns one.symbolic.expr)
(defrecord plus [operands])
(defrecord mult [operands])
(defrecord minus [operands])
(defrecord divide [operands])
(defrecord symb [sym])

(defmulti evalx (fn [expr sym-vals] (if (number? expr) :number (class expr))))

(defn arith-exp-evaluator [op op-identity commutative-op expr-creator]
  (fn evaluator [{:keys [operands]} sym-vals]
    (let [operands (if (> (count operands) 1) operands (cons op-identity operands))
          [f & rs] (mapv #(evalx % sym-vals)
                         (if (> (count operands) 1)
                           operands (cons op-identity operands)))
          {numbers true new-exprs false} (group-by number? rs)
          rs-acc (apply commutative-op numbers)]
      (if (number? f)
        (let [d (op f rs-acc)]
          (if (empty? new-exprs) d
              (expr-creator (cons d new-exprs))))
        (expr-creator (apply vector f rs-acc new-exprs))))))

(defn function-evaluator [expr-creator f]
  (fn f-evaluator [{:keys [operands]} sym-vals]
    (let [vals (mapv #(evalx % sym-vals) operands)]
      (if (every? number? vals) (apply f vals)
          (expr-creator vals)))))

(defmacro func-expr-creator [& math-func-names]
  (let [func-creator (fn [[s f]]
                       (let [creator-name (-> (str "->" (name s)) symbol)]
                         `((defrecord ~s [~'operands])
                           (let [evalx# (function-evaluator ~creator-name ~f)
                                 classname# (class (~creator-name []))]
                             (defmethod evalx classname# [expr# sym-vals#]
                               (evalx# expr# sym-vals#))))))
        all-defs (mapcat func-creator math-func-names)]
    `(do ~@all-defs)))

(let [sinf #(Math/sin %) cosf #(Math/cos %) logf #(Math/log %) powf #(Math/pow %1 %2) expf #(Math/exp %)]
  (func-expr-creator [sin sinf] [cos cosf] [log logf] [pow powf] [exp expf]))

(let [evalx-p (arith-exp-evaluator + 0 + ->plus)]
  (defmethod evalx plus [expr sym-vals]
    (evalx-p expr sym-vals)))

(let [evalx-sub (arith-exp-evaluator - 0 + ->minus)]
  (defmethod evalx minus [expr sym-vals]
    (evalx-sub expr sym-vals)))

(let [evalx-mul (arith-exp-evaluator * 1 * ->mult)]
  (defmethod evalx mult [expr sym-vals]
    (evalx-mul expr sym-vals)))

(let [evalx-div (arith-exp-evaluator / 1 * ->divide)]
  (defmethod evalx divide [expr sym-vals]
    (evalx-div expr sym-vals)))

(defmethod evalx symb [expr sym-vals]
  (let [sym (:sym expr)]
    (if-let [val (sym sym-vals)]
      val expr)))

(defmethod evalx :number [expr sym-vals]
  expr)