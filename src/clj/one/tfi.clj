(ns one.tfi)
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
                         (helper (dec c-num-deriv)
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
                                                           [1. 0. 0. 1.] [0. 0. 0. 0.]) v)]
                                       (assert (every? #(< (abs %) eps) errors))
                                       (recur (inc c-derivative-order) (map derivative [cp1 cp2]))))))]
     (every? identity (map verify-nth-pair (range num-boundary-continuities) polynomials)))))

#_(hermite-polynomial-tester 6)

(defn poly-linear [v]
  (if (number? v) (constantly v)
      (let [{min-fn :min max-fn :max} (into {}
                                            (map (fn [[min-max-key v]]
                                                   [min-max-key (poly-linear v)]) v))]
        " needs some work to use reshape to improve performance "
        (fn [c & rest-of-coords]
          (let [min-v (apply min-fn rest-of-coords)
                max-v (apply max-fn rest-of-coords)]
            (+ (* (- 1 c) min-v) (* c max-v)))))))

(defn reshape [mp reordered-indices]
  "making the equal depth assumption ...."
  (let [kys (reverse (loop [kys nil cmp mp]
                       (let [n-kys (conj kys (keys cmp))
                             n-cmp (cmp (ffirst n-kys))]
                         (if-not (map? n-cmp) n-kys
                                 (recur n-kys n-cmp)))))]
    (reduce (fn [cmp old-key]
              (let [new-key (mapv #(nth old-key %1) reordered-indices)
                    value (get-in mp old-key)]
                (assoc-in cmp new-key value))) 
            {} (apply cmb/cartesian-product kys))))

#_ (def e (reshape {:a0 {:b0 {:c0 10 :c1 20}
                         :b1 {:c0 30 :c1 40}}
                    :a1 {:b0 {:c0 50 :c1 60}
                         :b1 {:c0 70 :c1 80}}}
                   [2 0 1]))

#_ (def d (poly-linear {:min {:min {:min 10 :max 20}
                              :max {:min 30 :max 40}}
                        :max {:min {:min 50 :max 60}
                              :max {:min 70 :max 80}}}))

(defn curry-call [f & args]
  (let [{:keys [bounded unbounded] :as new-meta-info}
        (reduce (fn [{:keys [bounded unbounded] :as meta-info} [k v]]
                  (if (bounded k) (throw (str k " is already bound"))
                      (-> meta-info
                          (assoc-in [:bounded k] v)
                          (update-in [:unbounded] disj k))))
                (or (meta f) {}) (partition 2 args))]
    (if (empty? unbounded) (apply f (apply concat (seq bounded)))
        (with-meta f new-meta-info))))

(defn make-curryable [f & necessary-args-set]
  (with-meta f {:unbounded {} :bounded (set necessary-args-set)}))

(defn generate-faces [n m dim-keys]
  " generate 'm-faces' in an n-dimensional hypercube "
  (let [list-of-face-vars (cmb/combinations dim-keys m)
        hypercube-faces (fn [face-vars]
                          (let [var-dir-set (set face-vars)]
                              (->> dim-keys
                                   (map #(if (var-dir-set %) [[% :var]] [[% :min] [% :max]]))
                                   (apply cmb/cartesian-product)
                                   (map #(into {} %)))))]
    (map hypercube-faces list-of-face-vars)))

(defn tfi-fn [dir-specs]
  {:pre (every? (fn [[dir {:keys [func-basis-quadruplets]}]]
                  (and dir func-basis-quadruplets
                       (every? (fn [{:keys [f-min phi-min f-max phi-max]}]
                                 (and f-min phi-min f-max phi-max))
                               func-basis-quadruplets)))
                dir-specs)}
  (fn [& {:as params}]
    (let [dir-keys (keys dir-specs)
          calc-function (fn []
                          (let [a (fn [f & [first-dir & rest-of-dirs-to-constrain :as dirs-to-constrain]]
                                    (apply make-curryable
                                           (get-in dir-specs [first-dir :func-basis-quadruplets]) dirs-to-constrain))]))]
      (reduce (fn [val num-coords-to-choose]
                (reduce (fn [c-val list-of-interpolant-dir-groups]
                          (reduce (fn [c-c-val variable-keys]
                                    )
                                  c-val list-of-interpolant-dir-groups))
                        val (generate-faces (count dir-keys) num-coords-to-choose dir-keys)))
              0.0 (range (count dir-keys))))))


(let [dir-keys (mapv #(-> % (+ (int \a)) char str keyword) (range 0 26))]
  (defn random-tfi
    ([n dim-keys num-derivatives-to-impose] " range of all dimensions is in 0 to 1 "
       (let [rand-derivative (fn [derivative-level]
                               [[:derivative derivative-level]
                                (into {}
                                      (map #(vector % (constantly (rand)))
                                           (cmb/combinations dim-keys derivative-level)))])
             boundary-fns (into {} (map #(into {} (map rand-derivative [0 1]))
                                        (apply concat (generate-faces n 0 dim-keys))))]
         (reduce (fn [boundary-fns dim-id]
                   (reduce (fn [cur-boundary-fns boundary-key]
                             (let [gen-boundary-fn (fn [boundary-key]
                                                     (let [var-keys (keep (fn [[k v]] (if (= v :var) k)) boundary-key)]
                                                       (tfi-fn (into {}
                                                                     (map (fn [dir]
                                                                            (let [h #(cur-boundary-fns (assoc boundary-key dir %))]
                                                                              [dir {:range [0.0 1.0]
                                                                                    :func-basis-quadruplets [{:f-min (h :min)
                                                                                                              :f-max (h :max)
                                                                                                              :phi-min :linear-0-1
                                                                                                              :phi-max :linear-1-0}]}]))
                                                                          var-keys)))))]
                               (assoc cur-boundary-fns boundary-key (gen-boundary-fn boundary-key))))
                           (apply concat (generate-faces n dim-id dim-keys))))
                 (range 1 n))))))



#_(def tfi-2d-1 (tfi-fn [{:dir-id :u :range [0 1]
                          :func-basis-quadruplets [{:f-min (fn [{:keys [u v]}] 10.0)
                                                    :phi-min linear-0-1
                                                    :f-max (fn [{:keys [u v]}] 20.0)
                                                    :phi-max linear-1-0}]}
                         {:dir-id :v :range [0 1]
                          :func-basis-quadruplets [{:f-min (fn [{:keys [u v]}] 30.0)
                                                    :phi-min linear-0-1
                                                    :f-max (fn [{:keys [u v]}] 40.0)
                                                    :phi-max linear-1-0}]}]))

(defn tfi [& surf-pairs]
  (let [dims (count surf-pairs)]
    (fn [& coords]
      (loop [c coords]))))