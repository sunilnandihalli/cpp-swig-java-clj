(ns one.core
  (:require [[shake.core :as s]
             [clojure.java.io :as io]]))


(def ^:dynamic *swig-includes*)
(def ^:dynamic *java-includes*
  (str " -I /usr/lib/jvm/java-6-openjdk/include "
       " -I /usr/lib/jvm/java-6-openjdk/include/linux "))

(def ^:dynamic *c-flags* " -c -fPIC ")
(def ^:dynamic *java-flags* "")
(def ^:dynamic *shared-object-location* "generated_shared_native/")

(defn stdout [p]
  (-> (.getInputStream p) io/reader))

(defn stderr [p]
  (-> (.getErrorStream p) io/reader))

(defn stdin [p]
  (-> (.getOutputStream p) io/writer))

(defn compile-swig-file [swg-file generated-file-output-dir]
  (s/swig -c++ -java -outdir generated-file-output-dir swig-file))

(defn compile-c++-swig-wrapper-file [c++-wrapper-file]
  (s/g++ *c-flags* c++-wrapper-file))

(defn compile-java-proxy-class [java-proxy-class-file]
  (s/javac *java-flags))