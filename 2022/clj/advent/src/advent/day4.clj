(ns advent.day4
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn is-superset
  [l1 h1 l2 h2]
  (if (or (and (<= l1 l2) (>= h1 h2)) (and (<= l2 l1) (>= h2 h1)))
    1
    0))

;; TODO
(defn has-overlap
  [l1 h1 l2 h2]
  (if (and (>= h1 l2) (<= l1 h2))
    1
    0))

(defn count-overlaps
  [[l1 h1 l2 h2 & rest]]
  (if rest
    (+ (has-overlap l1 h1 l2 h2) (count-overlaps rest))
    (has-overlap l1 h1 l2 h2)))

(defn count-supersets
  [[l1 h1 l2 h2 & rest]]
  (if rest
    (+ (is-superset l1 h1 l2 h2) (count-supersets rest))
    (is-superset l1 h1 l2 h2)))

(defn solve-day4 []
  (let [parsed-input (->> (slurp "src/advent/inputs/input4.txt")
                          (str/split-lines)
                          (mapv #(str/split % #"[,-]+"))
                          (flatten)
                          (mapv #(Integer/parseInt %)))]
    (println (count-supersets parsed-input))
    (println (count-overlaps parsed-input))))

(comment (solve-day4))
