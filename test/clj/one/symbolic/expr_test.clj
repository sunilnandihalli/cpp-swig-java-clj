(ns one.symbolic.expr-test
  (:require [one.symbolic expr])
  (:use [one.symbolic expr]
        [midje.sweet])
  (:import [one.symbolic.expr plus sine mult sub divide pow log symb]))

(let [s (plus [(symb. :a) (symb. :b)])]
  (fact
   (evalx s {:a 10 :b 20}) => 30))