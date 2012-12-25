(ns one.hermite-test
  (:use [one.hermite])
  (:use [midje sweet]))

(fact
 (hermite-polynomial-tester 6) => true)

(fact
 (hermite-polynomial-tester 2) => true)

(fact
 (hermite-polynomial-tester 4) => true)

(fact
 (hermite-polynomial-tester 8) => true)

(fact
 (hermite-polynomial-tester 10) => true)

