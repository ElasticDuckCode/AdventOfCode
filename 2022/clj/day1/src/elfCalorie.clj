(ns elfCalorie
  (:require [clojure.string :as str]))

(defn process-elf-calories [data]
  (->> data
       (str/split-lines)
       (map #(read-string %))
       (reduce +)))

(def elf-calorie-data (str/split (slurp "input.txt") #"\r?\n\n"))
(def elf-calories (mapv process-elf-calories elf-calorie-data))

(defn -main []
  (println (reduce max elf-calories))
  (println (reduce + (take 3 (sort > elf-calories)))))
