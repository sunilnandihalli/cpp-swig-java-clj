(ns one.core
  (:require [clojure.pprint :as p])
  (:import  complex complexDouble complexInt vecInt
            misc_utilsJNI runme))

(comment
  (let [p (System/getProperties)
        keys (.keys p)]
    (loop []
      (if (.hasMoreElements keys)
        (let [key (.nextElement keys) value (.get p key)]
          (p/pprint {:key key :value value})
          (recur))))))

(comment (System/loadLibrary "misc_utils_java")
         (System/loadLibrary "misc_utils"))

(misc_utils/fact 10)

(let [x (doto (complex. 10.0 20.0)
          (.setRe 100.0)
          (.setIm 200.0))]
  [(.getRe x) (.getIm x)])
