(defproject secured "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :resource-paths ["shared" "resources"] 
  :license {:name "Eclipse Public License"
  :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xms8G" "-Xmx8G" "-Xss8G" "-server"]
  :dependencies [[org.clojure/clojure "1.8.0"]])
