(ns one.stencil)

(defn all-possible-ways-to-sum [n m]
  {:doc " n : sum  .... m : num components"
   :post [(every? (fn [x] (= (apply + x) n)) %)]}
  (if-not (> m 0) []
   (map #(map (fn [[x y]] (- y x 1)) (partition 2 1 [(+ m n -1)] (cons -1 %)))
        (cmb/combinations (range (+ n m -1)) (- m 1)))))

(defn coefficient-fn [derivative-orders]
  (fn [deltas]
    (reduce * 
            (map (fn [d n] (reduce (fn [c i] (/ (* c d) i)) 1 (range 1 (inc n))))
                 deltas derivative-orders))))

(defn taylor-series-coefficients [n]
  (mapcat #(map (juxt vec coefficient-fn) %)
          (map #(all-possible-ways-to-sum % n) (range))))

(defn eye [n]
  (mapv (fn [i] (assoc (vec (repeat n 0)) i 1)) (range n)))

(defn pm [mat]
  (let [str-m (map #(map (fn [x]
                           (with-out-str (print (cond
                                                 (ratio? x) x
                                                 (integer? x) (int x)
                                                 :else x)))) %) mat)
        max-width (apply max (map #(apply max (map count %)) str-m))
        space-n (fn [n] (apply str (repeat n \space)))
        center-str (fn [s w] (let [s-len (count s)
                                   diff (- w s-len)
                                   pad-after (int (/ diff 2))
                                   pad-before (- diff pad-after)]
                               (str (space-n pad-before) s (space-n pad-after))))
        m (map #(map (fn [s] (center-str s max-width)) %) str-m)]
    (->> (-> (map #(apply str (interpose \space  %)) m)
             (interleave (repeat \newline)))
         (apply str)
         print)))

(defn random-matrix [m n]
  (vec (repeatedly m #(vec (repeatedly n (fn [] (/ (rand-int 20) (+ 1 (rand-int 20)))))))))

(defn multi-split [v & widths]
  (loop [c-v v [w & ws] widths result nil]
    (if-not w (reverse (cons c-v result))
            (recur (drop w c-v) ws (cons (take w c-v) result)))))

(defn horizontally-split [m & split-widths]
  (apply map vector
         (map #(map vec
                    (apply multi-split % split-widths)) m)))

(defn horizontally-juxtapose [& ms]
  (apply mapv #(vec (apply concat %&)) ms))

(defn swap-row [m row-1 row-2]
  (assoc m row-1 (m row-2) row-2 (m row-1)))

(defn swap-coll [m coll-1 coll-2]
  (mapv #(assoc % coll-1 (% coll-2) coll-2 (% coll-1)) m))

(defn row-compute [m rf & {:as row-coeff-pairs}]
  (let [[row-ids coeffs] (apply map vector row-coeff-pairs)]
    (assoc m rf
           (apply mapv #(apply + (map * coeffs %&))
                  (map m row-ids)))))

(defn pivot-row-coll-id [m r-id c-id]
  (let [num-rows (count m)]
    (loop [c-m m c-r-id (inc r-id)]
      (if-not (< c-r-id num-rows) c-m
        (let [c-r (c-m c-r-id) r (c-m r-id)]
          (if (<= (abs (c-r c-id)) (abs (r c-id)))
            (recur c-m (inc c-r-id))
            (recur (swap-row c-m c-r-id r-id) (inc c-r-id))))))))

(defn forward-eliminate [m r-id c-id]
  (let [num-rows (count m)
        m (pivot-row-coll-id m r-id c-id)
        factor ((m r-id) c-id)]
    (if (= factor 0) {:next-row-id r-id :next-coll-id (inc c-id) :cur-m m}
        (loop [c-m (row-compute m r-id r-id (/ 1 factor)) c-r-id (inc r-id)]
          (if-not (< c-r-id num-rows) {:next-row-id (inc r-id) :next-coll-id (inc c-id) :cur-m c-m}
                  (recur (row-compute c-m c-r-id r-id (- ((c-m c-r-id) c-id)) c-r-id 1) (inc c-r-id)))))))

(defn sort-vector-by-non-zero-vector-length [m]
  (let [non-zero-count (fn [[id x]]
                         (let [vx (vec x)]
                           (loop [[v & vs] (rseq vx) n (count vx)]
                             (if-not (and v (zero? v)) n
                                     (recur vs (dec n))))))
        [sorted-coll-ids transposed-matrix] (apply map vector
                                                   (sort-by non-zero-count
                                                            (map-indexed vector
                                                                         (apply mapv vector m))))]
    {:ids sorted-coll-ids :matrix (apply mapv vector transposed-matrix)}))

(defn reverse-eliminate [m r-id c-id]
  (let [num-rows (count m)
        top-left-val ((m r-id) c-id)]
    (loop [c-m m c-r-id (dec r-id)]
      (if (< c-r-id 0) c-m
          (recur (row-compute c-m c-r-id r-id
                              (- (get-in c-m [c-r-id c-id])) c-r-id 1)
                 (dec c-r-id))))))

(defn random-upper-triangular-matrix [m n]
  (apply mapv vector
         (mapv (fn [[r c]]
                 (vec (take m
                            (concat
                             (repeatedly (dec r) #(rand-int 20)) [1] (repeat 0)))))
               (loop [r-id 0 c-id 0 r-c-pairs nil]
                 (if (= c-id n) (reverse r-c-pairs)
                     (recur (if (zero? (rand-int 3)) r-id
                                (let [n-r-id (inc r-id)]
                                  (if (> n-r-id m) m n-r-id)))
                            (inc c-id) (cons [r-id c-id] r-c-pairs)))))))

(defn dash-sep [n]
  (println (apply str (repeat n \-))))

(defn guass-eliminate [m]
  (let [num-rows (count m)
        num-colls (count (first m))
        coeffs-width (- num-colls num-rows)
        f-eliminated-m (loop [{:keys [next-row-id next-coll-id cur-m] :as w}
                              {:next-row-id 0 :next-coll-id 0 :cur-m m}]
                         (if-not (< next-row-id num-rows) (:cur-m w)
                                 (recur (forward-eliminate cur-m next-row-id next-coll-id))))
        [taylor-series-coefficient-matrix locs-coefficient-matrix] (horizontally-split f-eliminated-m coeffs-width)
        {:keys [ids matrix]} (sort-vector-by-non-zero-vector-length taylor-series-coefficient-matrix)
        concated-ids (vec (concat ids (range coeffs-width num-colls)))
        to-be-backward-substituted-m (horizontally-juxtapose matrix locs-coefficient-matrix)
        back-substituted-m (loop [r-id (dec num-rows) c-id (dec coeffs-width) cur-m to-be-backward-substituted-m]
                             (if (or (< r-id 0) (< c-id 0)) cur-m
                                 (let [v (get-in cur-m [r-id c-id])]
                                   (if (zero? v)
                                     (recur (dec r-id) c-id cur-m)
                                     (let [v-left (get-in cur-m [r-id (dec c-id)])]
                                       (if (or (and v-left (zero? v-left)) (nil? v-left))
                                         (recur (dec r-id) (dec c-id) (reverse-eliminate cur-m r-id c-id))
                                         (recur r-id (dec c-id) cur-m)))))))]
    {:reordered-ids concated-ids :matrix back-substituted-m}))

(defn highest-derivative-orders-possible [locs]
  (apply map #(-> %& set count dec) locs))

(defn is-derivative-estimate-possible? [locs deriv]
  (let [h (highest-derivative-orders-possible locs)]
    (every? identity (map #(<= %1 %2) deriv h))))

(defn stencils-str [m deriv-ids locs]
  (let [num-deriv-ids (count deriv-ids)
        num-locs (count locs)
        prods-str (fn [coef param]
                    (interpose " + "
                               (keep identity
                                     (map #(if-not (zero? %1)
                                             (if (= 1 %1) (str %2)
                                               (str %1 " . " %2)))
                                          coef param))))
        stencil-str (fn [row]
                      (let [[deriv-coeffs loc-coeffs] (split-at num-deriv-ids row)]
                        (apply str (concat (prods-str deriv-coeffs deriv-ids) [" = "] (prods-str loc-coeffs locs)))))]
    (apply str (interleave (map stencil-str m) (repeat \newline)))))

(defn stencil [locs]
  (let [n-dims (count (first locs))
        taylor-terms (filter #(is-derivative-estimate-possible? locs (first %))
                             (take-while (let [mx-drv-ordr (apply + (highest-derivative-orders-possible locs))]
                                           (fn [[deriv _]] (<= (apply + deriv) mx-drv-ordr)))
                                         (taylor-series-coefficients n-dims)))
        [deriv-ids coeff-fns] (apply mapv vector taylor-terms)
        coeff-matrix (mapv (fn [loc] (mapv #(% loc) coeff-fns)) locs)
        loc-matrix (eye (count locs))
        final-matrix (horizontally-juxtapose coeff-matrix loc-matrix)
        {:keys [reordered-ids matrix]} (guass-eliminate final-matrix)
        reordered-deriv-ids (mapv deriv-ids (take (count deriv-ids) reordered-ids))]
    (print (stencils-str matrix reordered-deriv-ids locs))))

#_ (stencil [[1] [-1]])
#_ (stencil [[0] [1] [-1]])
#_ (stencil [[0 0] [0 1] [1 0] [-1 0] [0 -1]]) 
#_ (stencil [[0 0] [0 1] [0 2] [0 3] [0 4] [1 0] [-1 0] [0 -1]]) 
#_ (stencil [[0] [1] [-1]])
#_ (stencil [[0 0] [1 1] [-1 1] [1 -1] [-1 -1]]) 
#_ (stencil [[1 1] [-1 1] [1 -1] [-1 -1]]) 
#_ (stencil (for [a (range 3) b (range 3)] [a b]))
#_ (stencil (for [a (range -1 2) b (range -1 2) c (range -1 2)] [a b c]))