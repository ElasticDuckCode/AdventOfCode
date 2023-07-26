(ns advent.day2
  (:require [clojure.string :as str]))

(defn process-strategy ;; TODO
  [[move & moves]]
  (let [got (get move 0) sent (get move 2)]
    (println got sent)))

(defn solve-day2 []
  (let [input (slurp "input2.txt")]
    (process-strategy (str/split-lines input))))

