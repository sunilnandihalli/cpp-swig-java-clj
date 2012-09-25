(ns one.core
  (:require [shake.core :as s]
            [clojure.java.io :as io]))


(def ^:dynamic *swig-includes*)
(def ^:dynamic *java-includes*
  (str " -I /usr/lib/jvm/java-6-openjdk/include "
       " -I /usr/lib/jvm/java-6-openjdk/include/linux "))

(def ^:dynamic *c-flags* "-ggdb3 -c -fPIC ")
(def ^:dynamic *cxx-flags* " -ggdb3 ")
(def ^:dynamic *java-flags* "")
(def ^:dynamic *swig-interface-files* "src/swig_files/")
(def ^:dynamic *shared-object-location* "generated_shared_native/")
(def ^:dynamic *proxy-classes-location* "proxy_classes/")
(def ^:dynamic *library-to-link* "")
(def ^:dynamic *generated-cxx-file-output-dir* "src/gen_cpp")
(def ^:dynamic *generated-java-file-output-dir* "src/gen_java")
(def ^:dynamic *cxx-include-files*)
(defn stdout [p]
  (-> (.getInputStream p) io/reader))

(defn stderr [p]
  (-> (.getErrorStream p) io/reader))

(defn stdin [p]
  (-> (.getOutputStream p) io/writer))

(defmacro echo [form]
  (let [syms (map #(let [form_term  (name %)]
                     (if (= \$ (first form_term)) [:expand (symbol (apply str (rest form_term)))] [:as-is form_term])) form)
        print_stmts (map (fn [x] `(print ~x)) (interpose " " (map second syms)))]
    `(let [p# ~form]
       (print \() ~@print_stmts (println \))
       (print (slurp (stdout p#)))
       (print (slurp (stderr p#)))
       p#)))

(defn compile-swig-file [swg-file]
  (echo (s/swig -c++ -java -ignoremissing -outdir $*generated-java-file-output-dir* $swg-file)))

(defn compile-c++-swig-wrapper-files [cxx-wrapper-files]
  (doseq [cxx-wrapper-file cxx-wrapper-files]
    (echo (s/g++ $*c-flags* $cxx-wrapper-file))))

(defn compile-java-proxy-class [java-proxy-class-file]
  (echo (s/javac $*java-flags* $java-proxy-class-file -d $*proxy-classes-location*)))

(defn link-shared-wrapper-objects [list-of-objects shared-object-fname]
  (let [list-of-objects-str (apply str (interpose ' ' list-of-objects))]
    (echo
     (s/g++ -shared $list-of-objects-str $*library-to-link* -o $shared-object-fname))))

(defn build-wrappers [swig-interface-file-list]
  (doseq [x swig-interface-file-list]
    (compile-swig-file x)))

(defn list-files
  ([directory regex]
     (let [p (s/ls $directory)
           ls-out (slurp (stdout p))]
       (->> (seq (.split ls-out "\n"))
            (remove clojure.string/blank?)
            (filter #(re-seq regex %))
            (map #(str directory "/" %)))))
  ([directory] (list-files #"")))

(defn build []
  (build-wrappers (list-files *swig-interface-files* #"\.i$"))
  (compile-c++-swig-wrapper-files (list-files *swig-interface-files* #"\.cxx")))