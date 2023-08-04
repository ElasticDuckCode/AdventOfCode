(ns advent.day8
  (:require
   [clojure.string :as str]
   [advent.matrix :refer [get-row
                          get-col
                          get-element
                          size
                          zeros
                          reduce-matrix]]))

(load "matrix")

;; helpers
(defn build-grid [input]
  (->> input
       (str/split-lines)
       (mapv #(str/split % #""))
       (mapv (fn [x] (mapv #(Integer/parseInt %) x)))))

(defn is-tree-visable [tree-grid ith jth]
  (let [row (get-row tree-grid ith)
        col (get-col tree-grid jth)
        height (get-element tree-grid ith jth)]
    (or (= ith 0)
        (= jth 0)
        (= ith (dec (size tree-grid 0)))
        (= jth (dec (size tree-grid 1)))
        (every? #(> height %) (subvec row 0 jth))
        (every? #(> height %) (subvec row (inc jth)))
        (every? #(> height %) (subvec col 0 ith))
        (every? #(> height %) (subvec col (inc ith))))))

(defn get-viewing-distance [trees, height]
  (let [view-distance (first (keep-indexed (fn [i val] (when (>= val height) i)) trees))]
    (if (nil? view-distance)
      (count trees)
      (inc view-distance))))

(defn score-scenic [tree-grid ith jth]
  (let [row (get-row tree-grid ith)
        col (get-col tree-grid jth)
        height (get-element tree-grid ith jth)]
    (reduce *
            [(get-viewing-distance (subvec row (inc jth)) height)
             (get-viewing-distance (subvec col (inc ith)) height)
             (get-viewing-distance (reverse (subvec row 0 jth)) height)
             (get-viewing-distance (reverse (subvec col 0 ith)) height)])))

(defn create-visable-grid
  ([tree-grid] (create-visable-grid tree-grid 0 0 (zeros (size tree-grid))))
  ([tree-grid ith jth visable-grid]
   (let [visable-grid (->> (is-tree-visable tree-grid ith jth)
                           (#(if % 1 0))
                           (assoc (get-row visable-grid ith) jth)
                           (assoc visable-grid ith))
         ith (+ ith (quot (inc jth) (size tree-grid 1)))
         jth (mod (inc jth) (size tree-grid 1))]
     (if (< ith (size tree-grid 0))
       (recur tree-grid ith jth visable-grid)
       visable-grid))))

(defn create-scenic-grid
  ([tree-grid] (create-scenic-grid tree-grid 0 0 (zeros (size tree-grid))))
  ([tree-grid ith jth scenic-grid] ;; TODO
   (let [scenic-grid (->> (score-scenic tree-grid ith jth)
                          (assoc (get-row scenic-grid ith) jth)
                          (assoc scenic-grid ith))
         ith (+ ith (quot (inc jth) (size tree-grid 1)))
         jth (mod (inc jth) (size tree-grid 1))]
     (if (< ith (size tree-grid 0))
       (recur tree-grid ith jth scenic-grid)
       scenic-grid))))

;; main for day8
(defn solve-day8 []
  (let [input (slurp "src/advent/inputs/input8.txt")
        tree-grid (build-grid input)
        visable-grid (create-visable-grid tree-grid)
        scenic-grid (create-scenic-grid tree-grid)]
    (println (reduce-matrix + visable-grid))
    (println (reduce-matrix max scenic-grid))))

(comment (solve-day8))
