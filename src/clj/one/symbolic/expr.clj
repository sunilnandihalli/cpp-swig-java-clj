(ns one.symbolic.expr)

(defprotocol symbolicExpr
  (evalx [this args]))

(defprotocol symbolicDifferentiable
  (differentiate [this & {:as derivative-orders-map}]))

(deftype sine [expr]
  symbolicExpr
  (evalx [this args]
    (let [s (evalx expr args)]
      (if (number? s) (Math/sin s)
          (sine. s)))))

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

(deftype sub [exprs]
  symbolicExpr
  (evalx [this args]
    (let [[f & rs] (map #(evalx % args) exprs)
          {numbers true new-exprs false} (group-by number? rs)
          sum-rs-nums (apply + numbers)]
      (if (number? f)
        (let [d (- f sum-rs-nums)]
          (if (empty? new-exprs) d
              (sub. (cons d new-exprs))))
        (sub. (apply vector f sum-rs-nums new-exprs))))))


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
  (evalx [this & {:as symb-val-map}]
    (if-let [x (keyword symb-val-map)] x this)))


#_ (def s) 