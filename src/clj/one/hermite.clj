(ns one.hermite
  (:require [clatrix.core :as m]))

(comment
  (def quadratic-0-1 (poly -1 0 1))
  (def quadratic-1-0 (poly 1 0 0))
  (def quadratic-derivative-0-1  (poly -1 1 0)))

(defprotocol polynomial
  (eval-poly [this t])
  (derivative [this])
  (nth-derivative [this n])
  (coeffs [this]))



(deftype poly [coeffs]
  polynomial
  (eval-poly [this t]
    (reduce #(+ (* %1 t) %2) 0.0 coeffs))
  (derivative [this]
    (poly. (map #(* %1 %2) coeffs (range (dec (count coeffs)) 0 -1))))
  (nth-derivative [this n]
    (loop [c-poly this n-left n]
      (if-not (> n-left 0) c-poly
              (recur (derivative c-poly) (dec n-left)))))
  (coeffs [this] coeffs))

(defn const-part-coeffs-of-nth-order-poly [num-deriv polynomial-order]
  (let [num-coeffs (inc polynomial-order)
        helper (fn helper [c-num-deriv factors-of-coeffs]
                 (if-not (> c-num-deriv 0) factors-of-coeffs
                         (recur (dec c-num-deriv)
                                (map #(* %1 %2) factors-of-coeffs
                                     (range (dec (count factors-of-coeffs)) 0 -1)))))]
    (vec (helper num-deriv (repeat num-coeffs 1.0)))))

(defn hermite-polynomials [num-boundary-continuities]
  (let [poly-order (- (* 2 num-boundary-continuities) 1)
        pad-zero #(take (inc poly-order) (concat % (repeat (inc poly-order) 0.0)))
        matrix-lhs (m/matrix
                    (mapcat #(let [l (dec (count %))]
                               (vector (pad-zero (concat (repeat l 0.0) [(% l)]))
                                       (pad-zero %)))
                            (for [deriv-level (range num-boundary-continuities)] 
                              (const-part-coeffs-of-nth-order-poly deriv-level poly-order))))
        matrix-rhs (m/id (inc poly-order))]
    (map (comp ->poly m/as-vec) (m/cols (m/solve matrix-lhs matrix-rhs)))))

(defn abs [x] (if (< x 0) (- x) x))
(let [eps 1e-6]
 (defn hermite-polynomial-tester [num-boundary-continuities]
   (let [polynomials (partition 2 (hermite-polynomials num-boundary-continuities))
         verify-nth-pair (fn [derivative-order [p1 p2]]
                           (loop [c-derivative-order 0 [cp1 cp2] [p1 p2]]
                             (if-not (< c-derivative-order num-boundary-continuities) true
                                     (let [v (for [cp [cp1 cp2] t [0.0 1.0]] (eval-poly cp t))
                                           errors (map - (if (= c-derivative-order derivative-order)
                                                           [1. 0. 0. 1.] [0. 0. 0. 0.]) v)
                                           all-errors-acceptable (every? #(< (abs %) eps) errors)]
                                       (println {:error (apply max (map abs errors)) :derivative-order derivative-order})
                                       (recur (inc c-derivative-order) (map derivative [cp1 cp2]))))))]
     (every? identity (map verify-nth-pair (range num-boundary-continuities) polynomials)))))
