(ns one.tfi-test
  (:use [one.tfi]
        [midje.sweet]))


(let [result {:c1 {:a1 {:b1 80, :b0 60}
                   :a0 {:b1 40, :b0 20}}
              :c0 {:a1 {:b1 70, :b0 50}
                   :a0 {:b1 30, :b0 10}}}
      input  {:a0 {:b0 {:c0 10 :c1 20}
                   :b1 {:c0 30 :c1 40}}
              :a1 {:b0 {:c0 50 :c1 60}
                   :b1 {:c0 70 :c1 80}}}]
  (fact
   (reshape input [2 0 1]) => result))


(let [f (poly-linear {:min {:min {:min 10 :max 20}
                            :max {:min 30 :max 40}}
                      :max {:min {:min 50 :max 60}
                            :max {:min 70 :max 80}}})]
  (fact
   (f 0 0 0) => 10 (f 0 0 1) => 20 (f 0 1 0) => 30 (f 0 1 1) => 40 (f 1 0 0) => 50 (f 1 0 1) => 60 (f 1 1 0) => 70 (f 1 1 1) => 80
   (f 1/2 0 0) => 30 (f 0 1/2 1) => 30 (f 1/2 1/2 1/2) => 45 (f 0 1/2 1/2) => 25 (f 1 1/2 1/2) => 65
   (+ (* 1/4 (f 0 0 1/2)) (* 3/4 (f 0 1 1/2))) => (f 0 3/4 1/2)))

(fact
 (mapv count (generate-faces 4 0 [:a :b :c :d])) => (repeat 1 16)
 (mapv count (generate-faces 4 1 [:a :b :c :d])) => (repeat 4 8)
 (mapv count (generate-faces 4 2 [:a :b :c :d])) => (repeat 6 4)
 (mapv count (generate-faces 4 3 [:a :b :c :d])) => (repeat 4 2)
 (mapv count (generate-faces 4 4 [:a :b :c :d])) => (repeat 1 1)
 (mapv count (generate-faces 3 0 [:a :b :c])) => (repeat 1 8)
 (mapv count (generate-faces 3 1 [:a :b :c])) => (repeat 3 4)
 (mapv count (generate-faces 3 2 [:a :b :c])) => (repeat 3 2)
 (mapv count (generate-faces 3 3 [:a :b :c])) => (repeat 1 1))

(let [myf (fn myf [& {:keys [a b c d]}]
            (+ a b c d))
      curryable-myf (make-curryable myf :a :b :c :d)]
  (fact
   (-> curryable-myf
       (curry-call :a 20)
       (curry-call :b 30)
       (curry-call :c 40)
       (curry-call :d 50)) => (myf :a 20 :b 30 :c 40 :d 50)))