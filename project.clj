(defproject one "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :source-paths ["src/clj"]
  :java-source-paths ["gen_java_src" "src/java"]
  :native-path "cpp_lib"
  :jvm-opts [~(str "-Djava.library.path=cpp_lib/:/usr/lib/jni/:" (System/getenv "LD_LIBRARY_PATH"))]
  
  :profiles {:dev {:dependencies [[midje "1.4.0"]]
                   :plugins [[lein-midje "2.0.1"]
                             [lein-guzheng "0.4.3"]]}}

  :test-paths ["test/clj/" ]

  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.unify "0.5.3"]
                 [org.clojure/core.match "0.2.0-alpha11"]
                 [org.clojure/core.logic "0.7.5"]
                 [org.clojure/math.combinatorics "0.0.3"]
                 [clatrix "0.1.0"]
                 [midje "1.4.0"]
                 #_([incanter/incanter-core "1.2.2"]
                      [incanter/incanter-charts "1.2.2"]
                        [incanter/incanter-processing "1.2.2"]) 
                 #_[shake "0.2.1"]
                 [com.stuartsierra/lazytest "1.2.3"]]
  :repositories {"stuart" "http://stuartsierra.com/maven2"
                 "incanter" "http://repo.incanter.org"})

