(ns advent.core
  (:gen-class)
  (:require [advent.day1 :as day1])
  (:require [advent.day2 :as day2]))

(defn -main []
  (println "day1: ")
  (day1/solve-day1)
  (println "day2: ")
  (day2/solve-day2))
