(defproject pushfight "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "FIXME"
            :url "FIXME"}
  :dependencies [[org.clojure/clojure "1.11.1"]

                 [com.rpl/specter "1.1.4"]
                 
                 [com.taoensso/telemere "1.0.0-RC1"]
                 [com.taoensso/nippy "3.4.2"]]

  :repl-options {:init-ns pushfight.core}
  :main pushfight.core)
