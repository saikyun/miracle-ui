(ns test-stuff.core
  (:require [miracle.save :as ms :refer-macros [save]]))

(defn start
  []
  (println "starting"))

(defn stop
  []
  (println "stopping"))

(defn main
  []
  (println "test-stuff main")
  (start))
