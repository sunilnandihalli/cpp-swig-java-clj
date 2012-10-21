(ns one.core
  (:require [clojure.pprint :as p])
  (:import  complex complexDouble complexInt vecInt
                        misc_utilsJNI runme))

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

(def args (make-array String 0))

(defn check-fn[cplx-type-ctor gen adder]
  (let [get-val #(vector (.getRe %) (.getIm %))
        [a b c d e f :as inp] (repeatedly 6 gen)
        x (doto (cplx-type-ctor a b)
            (.setRe c)
            (.setIm d))
        y (cplx-type-ctor e f)
        z (adder x y)]
    [inp (map get-val [x y z])]))

(defmacro check [cplx-type gen add_op]
  `(check-fn #(new ~cplx-type %1 %2) ~gen #(. %1 ~add_op %2)))


(defmacro interposing-doto [[java-obj interpose-form] & forms]
  (let [interposed-forms (interleave (map (fn [java-interop-form]
                                            `(#(doto % ~java-interop-form))) forms)
                                     (repeat interpose-form))]
    `(-> ~java-obj ~@interposed-forms)))



(defmacro bounded [& forms]
  (let [new-forms (map (fn [frm]
                         `(do
                            (println (str "starting .... " ~(str frm)))
                            ~frm
                            (println (str "finished ....." ~(str frm))))) forms)]
    `(do ~@new-forms)))
(bounded
 (jnative.runme/main args))

#_(bounded
   (misc_utils/fact 10)
   (runme/main args)
   (check complexInt #(rand-int 100) add_op)
   (check complexDouble rand add_op)
   (check complex rand add_op)
   (scaffold vecInt)
   (let [v (vecInt.)
         print-n-continue (fn [v]
                            (clojure.pprint/pprint
                             (for [i (range (.size v))]
                               (.get v i))) v)]
     (interposing-doto [v print-n-continue]
                       (.add 10) (.add 20) (.add 30) (.add 40) (.clear) (.reserve 100)))
   (callback.runme/main args)
   (extend.runme/main args)
   (reference.runme/main args)
   (jenum.runme/main args)         
   (jclass.runme/main args)
   (jtemplate.runme/main args)
   (simple.runme/main args)
   (constants.runme/main args)
   (variables.runme/main args)
   (pointer.runme/main args)
   (funcptr.runme/main args)
   (typemap.runme/main args)
   (multimap.runme/main args)
   #_(jnative.runme/main args))

(comment
  (defmacro check [cplx-type gen add_op]
    `(check-fn #(new ~cplx-type %1 %2) ~gen #(~(symbol (str "." (name add_op))) %1 %2))))

