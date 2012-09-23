(ns one.core
  (:require [[shake.core :as s]
             [clojure.java.io :as io]]))


(def ^:dynamic *swig-includes*)
(def ^:dynamic *java-includes*
  (str "-I /usr/lib/jvm/java-6-openjdk/include "
       "-I /usr/lib/jvm/java-6-openjdk/include/linux "))

(def )

(defn stdout [p]
  (-> (.getInputStream p) io/reader))

(defn stderr [p]
  (-> (.getErrorStream p) io/reader))

(defn stdin [p]
  (-> (.getOutputStream p) io/writer))

(defn compile-swig-file [swg-file generated-file-output-dir]
  (s/swig -c++ -java -outdir generated-file-output-dir swig-file))

(defn compile-java-swig-wrapper-file [])