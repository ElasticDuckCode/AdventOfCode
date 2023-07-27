(ns advent.day3
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn split-half [s]
  (split-at (/ (count s) 2) s))

(defn get-priority [s]
  (let [int-s (int s)]
    (if (> int-s 96)
      (- int-s 96)
      (+ (- int-s 64) 26))))

(defn sum-priorities [sacks]
  (reduce + (map (fn [x] (->> x
                              (split-half)
                              (map set)
                              (apply set/intersection)
                              (vec)
                              (first)
                              (get-priority))) sacks)))

(defn sum-badge-priorities [sacks]
  (->> sacks
       (partition 3)
       (map (fn [x] (->> x
                         (map set)
                         (apply set/intersection)
                         (vec)
                         (first)
                         (get-priority))))
       (reduce +)))

(defn solve-day3 []
  (let [input (->> (slurp "src/advent/inputs/input3.txt")
                   (str/split-lines)
                   (filter (fn [x] x)))]
    (println (sum-priorities input))
    (println (sum-badge-priorities input))))

