(defproject clj-aoc "0.1.0-SNAPSHOT"
  :description "Advent of Code Solutions in Clojure"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :main ^:skip-aot clj-aoc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
