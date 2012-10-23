(defproject one "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :java-source-paths ["gen_java_src" "src/java"]
  :native-path "cpp_lib"
  :jvm-opts [~(str "-Djava.library.path=cpp_lib/:" (System/getenv "LD_LIBRARY_PATH"))]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.unify "0.5.3"]
                 [org.clojure/core.match "0.2.0-alpha11"]
                 [org.clojure/core.logic "0.7.5"]
                 [shake "0.2.1"]])

