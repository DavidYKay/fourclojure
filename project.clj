(defproject fourclojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 ]
  :plugins [
            [lein-midje "3.0-alpha4"]
            ]
  
  :profiles {:dev {:dependencies [
                                  [midje "1.5-alpha10"]
                                  [com.stuartsierra/lazytest "1.2.3"]
                                  ]
                   }}
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"}
  )