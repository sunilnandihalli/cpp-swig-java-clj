(ns one.core
  (:require [clojure.pprint :as p])
  (:import complex complexDouble complexInt vecInt
           misc_utils))
(comment
  (let [p (System/getProperties)
        keys (.keys p)]
    (loop []
      (if (.hasMoreElements keys)
        (let [key (.nextElement keys)
              value (.get p key)]
          (p/pprint {:key key :value value})
          (recur))))))


(System/loadLibrary "wrapper")

(let [x (doto (complex. 10.0 20.0)
          (.setRe 10.0)
          (.setIm 20.0))]
  [(.getRe x) (.getIm x)])
