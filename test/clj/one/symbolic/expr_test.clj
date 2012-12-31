(ns one.symbolic.expr-test
  (:require [one.symbolic expr])
  (:use [one.symbolic expr]
        [midje.sweet])
  (:import [one.symbolic.expr plus sin cos mult minus divide pow log symb exp]))

(let [[a b c] (map ->symb [:a :b :c])
      s1 (plus. [a b])
      s2 (minus. [a b c])
      s3 (mult. [a b c])
      s4 (divide. [a b c])
      s5 (sin. [s1])
      s6 (log. [s1])
      s7 (pow. [s1 s4])
      s8 (minus. [a])
      s9 (plus. [a])
      s10 (mult. [a])
      s11 (divide. [a])
      s12 (plus. [])
      s13 (mult. [])
      s14 (plus. [1 2 3 4])
      s15 (minus. [1 2 3 4])
      s16 (mult. [1 2 3 4])
      s17 (divide. [1 2 3 4])]
  (fact
   (evalx (differentiate s2 :b) {}) => -1
   (evalx (differentiate s1 :a) {}) => 1 
   (evalx (differentiate s3 :b) {:a 2 :c 3}) => 6
   (evalx (differentiate s4 :a) {:a 2 :b 3 :c 5}) => 1/15
   (evalx (differentiate s4 :b) {:a 2 :b 3 :c 5}) => (evalx (minus. [(divide. [a b b c]) ]) {:a 2 :b 3 :c 5})
   (evalx (higher-order-derivative s4 {:a 1 :b 1}) {:a 2 :b 3 :c 5}) => (evalx (divide. [-1 b b c]) {:a 2 :b 3 :c 5})
   (evalx (higher-order-derivative s5 {:a 1 :b 2}) {:a 2 :b 3}) => (evalx (minus. [(cos. [(plus. [a b])])]) {:a 2 :b 3})
   (evalx s1 {:a 10 :b 20}) => 30
   (evalx s2 {:a 10 :b 20 :c 30}) => -40
   (evalx s3 {:a 2 :b -3 :c 0.5}) => -3.0
   (evalx s4 {:a 1 :b 2 :c -3}) => -1/6
   (evalx s5 {:a 0 :b 0}) => 0.0
   (evalx s6 {:a 3 :b -2 :c -5}) => 0.0
   (evalx s7 {:a 1 :b 2 :c 3}) => 1.2009369551760027
   (evalx s8 {:a 10}) => -10
   (evalx s9 {:a 10}) => 10
   (evalx s10 {:a 10}) => 10
   (evalx s11 {:a 10}) => 1/10
   (evalx s12 {:a 10}) => 0
   (evalx s13 {:a 10}) => 1
   (evalx s14 {}) => 10
   (evalx s15 {}) => -8
   (evalx s16 {}) => 24
   (evalx s17 {}) => 1/24
   (evalx (minus. [(minus. [1])]) {}) => 1
   (-> (evalx s2 {:a 10}) (evalx {:b 20}) (evalx {:c 30})) => -40
   (-> (evalx s2 {:a b}) (evalx {:c b}) (evalx {:b 20})) => -20))


(let [[a b c] (mapv #(symb. %) [:a :b :c])
      d (plus. [a (pow. [b c])])
      s (log. [d])
      s-a1 (divide. [d])
      s-a2 (divide. [-1 d d])
      s-a2-b1 (mult. [2 c (pow. [d -3]) (pow. [b (minus. [c 1])])])
      s-a2-b2 (plus. [(mult. [2 c (mult. [-3 (pow. [d -4]) c (pow. [b (minus. [c 1])])]) (pow. [b (minus. [c 1])])])
                      (mult. [2 c (pow. [d -3]) (minus. [c 1]) (pow. [b (minus. [c 2])])])])
      v {:a 5 :b 3 :c 2}]
  (fact 
   (evalx (higher-order-derivative s {:a 1}) v) => (evalx s-a1 v)
   (evalx (higher-order-derivative s {:a 2}) v) => (evalx s-a2 v)
   (evalx (higher-order-derivative s {:a 2 :b 1}) v) => (evalx s-a2-b1 v)
   (evalx (higher-order-derivative s {:a 2 :b 2}) v) => (evalx s-a2-b2 v)))

(let [[a b c] (mapv #(symb. %) [:a :b :c])
      s (exp. [a])
      v {:a 2}]
  (fact
   (evalx s v) => (Math/exp 2)
   (evalx (higher-order-derivative s {:a 1}) v) => (evalx s v)))