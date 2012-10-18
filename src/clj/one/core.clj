(ns one.core
  (:require [clojure.pprint :as p])
  (:import  complex complexDouble complexInt vecInt
            misc_utilsJNI runme
            ;callback.runme
            ;jenum.runme
            ;jclass.runme
            ;reference.runme
            ;extend.runme
            ))

(defn scaffold [iface]
  "this code is from Christophe Grand .. but very usefull.. so I chose to include.."
  (doseq [[iface methods] (->> iface .getMethods
                               (map #(vector (.getName (.getDeclaringClass %))
                                             (symbol (.getName %))
                                             (count (.getParameterTypes %))))
                               (group-by first))]
    (println (str "  " iface))
    (doseq [[_ name argcount] methods]
      (println
       (str "    "
            (list name (into ['this] (take argcount (repeatedly gensym)))))))))

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
          (.setIm 200.0))
      y (complex. 212.0 321.0)]
  [(.getRe x) (.getIm x) (.getRe y) (.getIm y)])
