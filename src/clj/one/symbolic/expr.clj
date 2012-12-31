(ns one.symbolic.expr
  (:use [clojure.pprint]))
(defrecord plus [operands])
(defrecord mult [operands])
(defrecord minus [operands])
(defrecord divide [operands])
(defrecord symb [sym])
(defn dispatch-fn [expr sym-vals] (if (number? expr) :number (class expr)))
(defmulti evalx dispatch-fn)
(defmulti differentiate dispatch-fn)
(defmulti simplify dispatch-fn)

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

(defn differentiation-chain-rule [operands vec-of-derivative-of-func-wrt-each-operand differentiating-sym]
  (let [operand-derivatives (mapv #(differentiate % differentiating-sym) operands)
        terms (vec (keep identity
                         (map (fn [op-derivative deriv-of-func-wrt-operand-fn]
                                (let [deriv-of-func-wrt-operand (deriv-of-func-wrt-operand-fn operands)]
                                  (if (every? #(not= 0 %) [op-derivative deriv-of-func-wrt-operand])
                                    (cond
                                     (= op-derivative 1) deriv-of-func-wrt-operand
                                     (= deriv-of-func-wrt-operand 1) op-derivative
                                     :else (mult. [op-derivative deriv-of-func-wrt-operand])))))
                              operand-derivatives vec-of-derivative-of-func-wrt-each-operand)))
        {numbers true new-exprs false} (group-by number? terms)
        terms-numeric-sum (apply + numbers)]
    (if (= 0 terms-numeric-sum)
      (cond
       (empty? new-exprs) 0
       (= 1 (count new-exprs)) (first new-exprs)
       :else (plus. new-exprs))
      (if (empty? new-exprs) terms-numeric-sum
          (plus. (cons terms-numeric-sum new-exprs))))))

(defn higher-order-derivative [expr deriv-order-map]
  (loop [[cur-sym order] nil cur-differentiated-expr expr
         next-deriv-order-map deriv-order-map]
    (cond
     (and cur-sym order (not= 0 order)) (recur [cur-sym (dec order)] (differentiate cur-differentiated-expr cur-sym) next-deriv-order-map)
     (not-empty next-deriv-order-map) (recur (first next-deriv-order-map) cur-differentiated-expr (rest next-deriv-order-map))
     :else cur-differentiated-expr)))

(let [derivative-funcs (repeat (constantly 1))]
  (defmethod differentiate plus [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [derivative-funcs (map (fn [index] (if (= 0 index)
                                          #(if (= 1 (count %)) -1 1)
                                          (constantly -1))) (range))]
  (defmethod differentiate minus [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [remove-item-at (fn [at coll]
                       (keep-indexed #(if (not= %1 at) %2) coll))
      derivative-funcs (map #(fn [operands]
                               (mult. (remove-item-at % operands))) (range))]
  (defmethod differentiate mult [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [derivative-funcs (cons (fn [operands]
                               (if (= 1 (count operands)) (divide. (cons -1 (repeat 2 (first operands))))
                                   (divide. (cons 1 (drop 1 operands)))))
                             (map (fn [index]
                                    (fn [operands]
                                      (divide. (->> (rest operands)
                                                    (cons (nth operands index))
                                                    (cons (minus. (take 1 operands))))))) (drop 1 (range))))]
  (defmethod differentiate divide [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [derivative-funcs [#(cos. %)]]
  (defmethod differentiate sin [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [derivative-funcs [#(minus. [(sin. %)])]]
  (defmethod differentiate cos [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [derivative-funcs [#(divide. %)]]
  (defmethod differentiate log [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [derivative-funcs [(fn [[x y]] (mult. [y (pow. [x (minus. [y 1])])]))
                        (fn [[x y]] (mult. [(pow. [x y]) (log. [x])]))]]
  (defmethod differentiate pow [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(let [derivative-funcs [#(exp. %)]]
  (defmethod differentiate exp [{:keys [operands]} deriv-sym]
    (differentiation-chain-rule operands derivative-funcs deriv-sym)))

(defmethod evalx symb [expr sym-vals]
  (if-let [val ((:sym expr) sym-vals)]
    val expr))

(defmethod differentiate symb [expr deriv-sym]
  (if (= (:sym expr) deriv-sym) 1 0))

(defmethod evalx :number [expr sym-vals]
  expr)

(defmethod differentiate :number [expr sym-vals]
  0)