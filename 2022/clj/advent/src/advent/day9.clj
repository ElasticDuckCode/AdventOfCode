(ns advent.day9
  (:require [advent.matrix :refer [zeros sign get-row]]
            [clojure.string :as str]))

(defn move-head [rope step]
  (assoc rope 0 (mapv + (get-row rope 0) step)))

(defn move-rest
  ([rope]
   (move-rest rope 1))
  ([rope k]
   (if (< k (count rope))
     (let [prev (get-row rope (dec k))
           curr (get-row rope k)
           diff (mapv - prev curr)
           curr (mapv + curr (sign diff))]
       (if-not (every? #(<= (abs %) 1) diff)
         (recur (assoc rope k curr) (inc k))
         (recur rope (inc k))))
     rope)))  ;;TODO

(defn move-rope [move rope visited]
  (if-not (every? zero? move)
    (let [rope (move-head rope (sign move))
          rope (move-rest rope)
          visited (conj visited (last rope))
          move (mapv - move (sign move))]
      (recur move rope visited))
    [rope visited]))

(defn simulate-motion [[move & moves] rope visited]
  (let [[rope visited] (move-rope move rope visited)]
    (if (pos? (count moves))
      (simulate-motion moves rope visited)
      [rope visited])))

(defn map-motion [[direction magnitude]]
  (let [magnitude (Integer/parseInt magnitude)]
    (case direction
      "U" [0 magnitude]
      "D" [0 (- magnitude)]
      "L" [(- magnitude) 0]
      "R" [magnitude 0])))

(defn solve-day9 []
  (let [input (slurp "src/advent/inputs/input9.txt")
        moves (->> input
                   (str/split-lines)
                   (mapv #(str/split % #"\s+"))
                   (mapv map-motion))
        part1 (simulate-motion moves (zeros [2 2]) #{[0,0]})
        part2 (simulate-motion moves (zeros [10 2]) #{[0,0]})]
    [(count (last part1)) (count (last part2))]))

(comment (solve-day9))
