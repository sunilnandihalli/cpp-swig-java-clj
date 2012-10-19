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
(def args (make-array String 0))
(runme/main args)
(defn check-fn[cplx-type-ctor gen]
  (let [[a b c d e f :as inp] (repeatedly 6 gen)
         x (doto (cplx-type-ctor a b)
             (.setRe c)
             (.setIm d))
         y (cplx-type-ctor e f)]
    [inp [(.getRe x) (.getIm x) (.getRe y) (.getIm y)]]))

(defmacro check [cplx-type gen]
  `(check-fn #(new ~cplx-type %1 %2) ~gen))

(check complex rand)
(check complexInt #(rand-int 100))
(check complexDouble rand)

(callback.runme/main args)
(extend.runme/main args)
(comment 
  (jenum.runme/main args)         
  (jclass.runme/main args)
  (reference.runme/main args))
