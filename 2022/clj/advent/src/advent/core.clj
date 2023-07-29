(ns advent.core
  (:gen-class)
  (:require [advent.day1 :as day1])
  (:require [advent.day2 :as day2])
  (:require [advent.day3 :as day3])
  (:require [advent.day4 :as day4]))

(defn -main []
  (println "day1: ")
  (day1/solve-day1)
  (println "\nday2: ")
  (day2/solve-day2)
  (println "\nday3: ")
  (day3/solve-day3)
  (println "\nday4: ")
  (day4/solve-day4))

