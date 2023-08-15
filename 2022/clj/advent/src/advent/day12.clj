(ns advent.day12
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn get-2d-index [idx n-col] [(quot idx n-col) (mod idx n-col)])

(defn find-val [grid ch]
  (get-2d-index (.indexOf (apply concat grid) ch) (count (get grid 0))))

(defn find-start [grid] (find-val grid \S))

(defn find-end [grid] (find-val grid \E))

(defn get-grid-val [grid pos] (get (get grid (first pos)) (last pos)))

(defn set-grid-val [grid pos v]
  (assoc grid (first pos) (assoc (get grid (first pos)) (last pos) v)))

(defn sort-dists [dists]
  (into
   (sorted-map-by (fn [k1 k2] (compare [(get dists k1) k1]
                                       [(get dists k2) k2])))
   dists))

(defn init-dists [grid start]
  (sort-dists
   (assoc (into {} (for [i (range (count grid))
                         j (range (count (first grid)))]
                     [[i j] ##Inf]))
          start 0)))

(defn visit-closest-unseen [dists seen]
  (if (contains? seen (first (first dists)))
    (recur (dissoc dists (first (first dists))) seen)
    (first (first dists))))

(defn calc-distance [cval nval]
  (if (or (nil? cval) (nil? nval))
    ##Inf
    (let [cint (if (= cval \S) 0 (- (int cval) (int \`)))
          nint (if (= nval \E) cint (- (int nval) (int \`)))
          dist (- nint cint)]
      (if (<= dist 1) dist ##Inf))))

(defn update-dist [dists prev seen pos value curr]
  (if-not (contains? seen pos)
    (if (contains? dists pos)
      (if (< value (get dists pos))
        [(assoc dists pos value), (assoc prev pos curr)]
        [dists prev])
      [dists prev])
    [dists prev]))

(defn update-dists [grid seen curr prev dists]
  (let [left (mapv + curr [0 -1])
        right (mapv + curr [0 1])
        up (mapv + curr [-1 0])
        down (mapv + curr [1 0])
        ldist (+ (get dists curr) (calc-distance
                                   (get-grid-val grid curr)
                                   (get-grid-val grid left)))
        rdist (+ (get dists curr) (calc-distance
                                   (get-grid-val grid curr)
                                   (get-grid-val grid right)))
        udist (+ (get dists curr) (calc-distance
                                   (get-grid-val grid curr)
                                   (get-grid-val grid up)))
        ddist (+ (get dists curr) (calc-distance
                                   (get-grid-val grid curr)
                                   (get-grid-val grid down)))]
    (->> [dists, prev]
         (apply #(update-dist %1 %2 seen left ldist curr))
         (apply #(update-dist %1 %2 seen right rdist curr))
         (apply #(update-dist %1 %2 seen up udist curr))
         (apply #(update-dist %1 %2 seen down ddist curr))
         (apply (fn [dists, prev] [(sort-dists dists) prev])))))

(defn climb-mountain
  ([grid]
   (let [start (find-start grid)
         end (find-end grid)]
     (climb-mountain grid end {} {start nil} (init-dists grid start))))
  ([grid goal seen prev dists]
   (let [curr (visit-closest-unseen dists seen)]
     (if (= curr goal)
       dists
       (let [seen (assoc seen curr true)
             [dists prev] (update-dists grid seen curr prev dists)
             dists (dissoc dists curr)]
         (pprint dists)
         (pprint goal)
         (recur grid goal seen prev dists))))))

(defn solve-day12 []
  (let [grid (->> (slurp "src/advent/inputs/input12_example.txt")
                  (str/split-lines)
                  (mapv char-array))]
    (climb-mountain grid)))

(comment (solve-day12)
         (calc-distance \S \a)
         (calc-distance \S \E)
         (calc-distance \d \E)
         (calc-distance \S \b)
         (calc-distance \a \c)
         (< ##Inf ##Inf))

