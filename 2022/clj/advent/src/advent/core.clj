(ns advent.core
  (:gen-class)
  (:require [advent.day1 :as day1]
            [advent.day2 :as day2]
            [advent.day3 :as day3]
            [advent.day4 :as day4]
            [advent.day5 :as day5]
            [advent.day6 :as day6]
            [advent.day7 :as day7]))

(defn -main []
  (println "day1: ")
  (day1/solve-day1)
  (println "\nday2: ")
  (day2/solve-day2)
  (println "\nday3: ")
  (day3/solve-day3)
  (println "\nday4: ")
  (day4/solve-day4)
  (println "\nday5: ")
  (day5/solve-day5)
  (println "\nday6: ")
  (day6/solve-day6)
  (println "\nday7: ")
  (day7/solve-day7))

