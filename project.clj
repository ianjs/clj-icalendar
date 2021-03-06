(defproject com.arteemus/clj-icalendar "0.1.4"
  :description "wrapper over ical4j to generate ics"
  :url "http://github.com/ianjs/clj-icalendar"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git"
        :url "https://github.com/ianjs/clj-icalendar"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.mnode.ical4j/ical4j "1.0.7"]
                 [clojure.java-time "0.3.0"]]
  :global-vars {*warn-on-reflection* false
                *assert* false})
