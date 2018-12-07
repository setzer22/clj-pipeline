(defproject clj-pipeline "0.2.0-SNAPSHOT"

 :description "A small DSL to specify pipelined computations over a local environment."
  :url "https://github.com/setzer22/clj-pipeline"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot clj-pipeline.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
