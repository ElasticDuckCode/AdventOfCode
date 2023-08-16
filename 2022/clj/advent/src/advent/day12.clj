(ns advent.day12
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [advent.matrix :refer [get-element infs]]))

(defn unwrap-index [idx n-col]
  [(quot idx n-col) (mod idx n-col)])

(defn find-val [grid ch]
  (unwrap-index (.indexOf (apply concat grid) ch) (count (get grid 0))))

(defn grid-to-map [grid]
  (let [m (count grid)
        n (count (first grid))]
    (into {}
          (apply concat
                 (for [i (range m)]
                   (for [j (range n)]
                     [[i j] (get-element grid i j)]))))))

(defn initial-distance [grid]
  (let [m (count grid)
        n (count (first grid))
        [i j] (find-val grid \S)]
    (grid-to-map
     (assoc (infs [m n]) i (assoc (vec (repeat n ##Inf)) j 0)))))

(defn calculate-distance [h1 h2]
  (let [h1 (if-not (= h1 \S) (int h1) (int \a))
        h2 (if-not (= h2 \E) (int h2) (int \z))
        d (- h2 h1)]
    (if (>= d 2) ##Inf (max d 1))))

(defn calculate-grid-distance [grid p1 p2]
  (calculate-distance (get-element grid (first p1) (last p1))
                      (get-element grid (first p2) (last p2))))

(defn update-distance [grid prev dist-map neighbor curr dist]
  (if (contains? dist-map neighbor)
    (let [old (get dist-map neighbor)
          new (+ dist (calculate-grid-distance grid curr neighbor))]
      (if (< new old)
        [(assoc dist-map neighbor new) (assoc prev neighbor curr)]
        [dist-map prev]))
    [dist-map prev]))

(defn follow-shortest-path [point prev path]
  (if-not (nil? point)
    (recur (get prev point) prev (conj path point))
    path))

(defn dijkstra-hillclimb [grid end dist-map prev]
  (if (pos? (count dist-map))
    (let [[m n] [(count grid) (count (first grid))]
          [[i j] dist] (apply min-key second dist-map)
          dist-map (dissoc dist-map [i j])
          [dist-map prev] (->> [dist-map prev]
                               (apply #(if (< (inc i) m)
                                         (update-distance grid %2 %1 [(inc i) j] [i j] dist)
                                         [%1 %2]))
                               (apply #(if (>= (dec i) 0)
                                         (update-distance grid %2 %1 [(dec i) j] [i j] dist)
                                         [%1 %2]))
                               (apply #(if (< (inc j) n)
                                         (update-distance grid %2 %1 [i (inc j)] [i j] dist)
                                         [%1 %2]))
                               (apply #(if (>= (dec j) 0)
                                         (update-distance grid %2 %1 [i (dec j)] [i j] dist)
                                         [%1 %2])))]
      (if (= [i j] end)
        (follow-shortest-path end prev [])
        (recur grid end dist-map prev)))
    nil))

(defn solve-day12 []
  (let [grid (->> (slurp "src/advent/inputs/input12.txt")
                  (str/split-lines)
                  (mapv char-array))
        path (dijkstra-hillclimb grid
                                 (find-val grid \E)
                                 (initial-distance grid)
                                 {(find-val grid \S) nil})]
    (dec (count path))))

(comment (solve-day12)
         (calculate-distance \a \a)
         (calculate-distance \a \b)
         (calculate-distance \a \c)
         (repeat 10 ##Inf)
         (infs [1 10]))

