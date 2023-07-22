(ns elfCalorie
  (:require [clojure.string :as str]))

(def elf-calorie-data (str/split (slurp "input.txt") #"\r?\n\n"))

(defn process-elf-calories [data]
  (->> data
       (str/split-lines)
       (map #(read-string %))
       (reduce +)))

(defn -main []
  (let [elf-calories (mapv process-elf-calories elf-calorie-data)]
    (println (reduce max elf-calories))
    (println (reduce + (take 3 (sort > elf-calories))))))
