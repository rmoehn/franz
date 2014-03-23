(defproject franz "0.1.0-SNAPSHOT"
  :description "Solution to exercise about funny words. See http://page.mi.fu-berlin.de/erlenhain/ExTwoRemoves."
  :url "https://github.com/rmoehn/franz"
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :main ^:skip-aot franz.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
