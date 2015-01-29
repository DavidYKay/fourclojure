(defproject fourclojure "0.1.0-SNAPSHOT"
  :description "Exercises from 4clojure.org"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.taoensso/timbre "2.6.1"]
                 ]
  :plugins [
            [lein-midje "3.1.3"]
            ]

  :profiles {:dev {:dependencies [
                                  [midje "1.6.3"]
                                  ]
                   }}
  )
