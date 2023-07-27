(ns advent.day2
  (:require [clojure.string :as str]))

(defn move-score [play]
  (let [us (get play 2)]
    (case us
      \X 1
      \Y 2
      \Z 3
      0)))

(defn win-score [play]
  (let [them (get play 0) us (get play 2)]
    (case them
      \A (case us \Y 6 \X 3 0)
      \B (case us \Z 6 \Y 3 0)
      \C (case us \X 6 \Z 3 0)
      0)))

(defn move-score-two [play]
  (let [them (get play 0) us (get play 2)]
    (case us
      \X (case them \A 3 \B 1 \C 2 0)
      \Y (case them \A 1 \B 2 \C 3 0)
      \Z (case them \A 2 \B 3 \C 1 0)
      0)))

(defn win-score-two [play]
  (let [us (get play 2)]
    (case us
      \X 0
      \Y 3
      \Z 6
      0)))

(defn calculate-score
  [[play & plays]]
  (if play
    (+ (move-score play) (win-score play) (calculate-score plays))
    0))

(defn calculate-score-two
  [[play & plays]]
  (if play
    (+ (move-score-two play) (win-score-two play) (calculate-score-two plays))
    0))

(defn solve-day2 []
  (let [input (->> (str/split-lines (slurp "src/advent/inputs/input2.txt"))
                   (filter (fn [x] x)))]
    (println (calculate-score input))
    (println (calculate-score-two input))))
