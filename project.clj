(defproject one "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :java-source-paths ["gen_java_src" "src/java"]
  :native-path "cpp_lib"
  :jvm-opts [~(str "-Djava.library.path=cpp_lib/:" (System/getenv "LD_LIBRARY_PATH"))]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [shake "0.2.1"]])

