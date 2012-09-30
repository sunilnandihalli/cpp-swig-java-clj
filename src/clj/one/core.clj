(ns one.core
  (:require [clojure.pprint :as p])
  (:import complex complexDouble complexInt vecInt
           misc_utils))


(System/loadLibrary "libwrapper.so")

(let [x (doto (complex. 10.0 20.0)
          (.setRe 10.0)
          (.setIm 20.0))]
  [(.getRe x) (.getIm x)])
