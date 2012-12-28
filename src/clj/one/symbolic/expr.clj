(ns one.symbolic.expr)

(defprotocol symbolicExpr
  (evalx [this args]))

(defprotocol symbolicDifferentiable
  (differentiate [this derivative-orders-map]))



(deftype sine [expr]
  symbolicExpr
  (evalx [this args]
    (let [s (evalx expr args)]
      (if (number? s) (Math/sin s)
          (sine. s)))))

(deftype cosine [expr]
  symbolicExpr
  (evalx [this args]
    (let [c (evalx expr args)]
      (if (number? c) (Math/cos c)
          (cosine. c)))))

#_(extend-type cosine
    symbolicDifferentiable
    (differentiate [this deriv-orders]
      (let [s (differentiate expr deriv-orders)]
        (if (number? s) (- (Math/sin s))
            (sine. c)))))

#_(extend-type sine
    symbolicDifferentiable
    (differentiate [this deriv-orders]
      (let [c ])))

(defn arith-exp-evaluator [op op-identity commutative-op expr-creator]
  (fn [{:keys [operands]} sym-vals]
    (let [[f & rs] (mapv #(evalx % sym-vals)
                         (if (> (count operands) 1)
                           operands (cons op-identity operands)))
          {numbers true new-exprs false} (group-by number? rs)
          rs-acc (apply commutative-op numbers)]
      (if (number? f)
        (let [d (op f rs-acc)]
          (if (empty? new-exprs) d
              (expr-creator (cons d new-exprs)))))
      (expr-creator (apply vector f rs-acc new-exprs)))))

(defrecord plus [operands])
(defrecord mult [operands])
(defrecord minus [operands])
(defrecord divide [operands])

(defmulti evalx class)
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
(extend-type plus
  symbolicExpr
  (evalx))

(deftype plus [exprs]
  symbolicExpr
  (evalx [this args]
    (let [results (map #(evalx % args) exprs)
          {numbers true new-exprs false} (group-by number? results)
          sum (apply + numbers)]
      (if (empty? new-exprs) sum
          (plus. (conj new-exprs sum))))))

(deftype mult [exprs]
  symbolicExpr
  (evalx [this args]
    (let [results (map #(evalx % args) exprs)
          {numbers true new-exprs false} (group-by number? results)
          prod (apply * numbers)]
      (if (empty? new-exprs) prod
          (mult. (conj new-exprs prod))))))

(deftype minus [exprs]
  symbolicExpr
  (evalx [this args]
    (let [[f & rs] (map #(evalx % args) exprs)
          {numbers true new-exprs false} (group-by number? rs)
          sum-rs-nums (apply + numbers)]
      (if (number? f)
        (let [d (- f sum-rs-nums)]
          (if (empty? new-exprs) d
              (minus. (cons d new-exprs))))
        (minus. (apply vector f sum-rs-nums new-exprs))))))


(deftype divide [exprs]
  symbolicExpr
  (evalx [this args]
    (let [[f & rs] (map #(evalx % args) exprs)
          {numbers true new-exprs false} (group-by number? rs)
          prod-rs-nums (apply * numbers)]
      (if (number? f)
        (let [q (/ f prod-rs-nums)]
          (if (empty? new-exprs) q
              (divide. (cons q new-exprs))))
        (divide. (apply vector f prod-rs-nums new-exprs))))))

(deftype pow [base expt]
  symbolicExpr
  (evalx [this args]
    (let [[b e] (map #(evalx % args) [base expt])]
      (if (every? number? [b e]) (Math/pow b e)
          (pow. b e)))))

(deftype log [e1 e2]
  symbolicExpr
  (evalx [this args]
    (let [[e1 e2] (map #(evalx % args) [e1 e2])]
      (if (every? number? [e1 e2]) (/ (Math/log e1) (Math/log e2))
          (log. e1 e2)))))

(deftype symb [keyword]
  symbolicExpr
  (evalx [this symb-val-map]
    (if-let [x (keyword symb-val-map)] x this)))


#_ (def s) 