(ns advent.day1
  (:require [clojure.string :as str]))

(defn process-elf-calories [data]
  (->> data
       (str/split-lines)
       (map #(read-string %))
       (reduce +)))

(def elf-calorie-data (str/split (slurp "src/advent/inputs/input.txt") #"\r?\n\n"))
(def elf-calories (mapv process-elf-calories elf-calorie-data))

(defn solve-day1 []
  (println (reduce max elf-calories))
  (println (reduce + (take 3 (sort > elf-calories)))))
