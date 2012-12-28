(ns one.symbolic.expr-test
  (:require [one.symbolic expr])
  (:use [one.symbolic expr]
        [midje.sweet])
  (:import [one.symbolic.expr plus sine mult minus divide pow log symb]))

(let [s1 (plus. [(symb. :a) (symb. :b)])
      s2 (minus. [(symb. :a) (symb. :b) (symb. :c)])
      s3 (mult. [(symb. :a) (symb. :b) (symb. :c)])
      s4 (divide. [(symb. :a) (symb. :b) (symb. :c)])
      s5 (sine. s1)
      s6 (log. s1 s3)
      s7 (pow. s1 s4)
      s8 (minus. :a)
      s9 (plus. :a)
      s10 (mult. :a)
      s11 (divide. :a)]
  (fact
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
   (evalx s11 {:a 10}) => 1/10))