(ns advent.day13
  (:require
   [clojure.string :as str]))

(defn process-packet-pair [[x & xs] [y & ys]]
  ;;(println x y)
  (cond
    (every? integer? [x y]) (if (= x y) (recur xs ys) (< x y))
    (every? coll? [x y]) (let [result (process-packet-pair x y)]
                           (if (nil? result) (recur xs ys) result))
    (and (integer? x) (coll? y)) (let [result (process-packet-pair [x] y)]
                                   (if (nil? result) (recur xs ys) result))
    (and (coll? x) (integer? y)) (let [result (process-packet-pair x [y])]
                                   (if (nil? result) (recur xs ys) result))
    (and (nil? x) (not (nil? y))) true
    (nil? x) (if (every? nil? [xs ys]) nil (recur xs ys))
    :else false))

(defn compare-packet-data
  ([pairs] (compare-packet-data [] pairs))
  ([flags [[x y] & pairs]]
   ;;(println "--" x "-" y "--")
   (if-not (or (nil? x) (nil? y))
     (let [result (process-packet-pair x y)]
       ;;(println result)
       (recur (conj flags result) pairs))
     flags)))

(defn solve-day13 []

  [;; part 1
   (->> (slurp "src/advent/inputs/input13.txt")
        (#(str/split % #"\n\n"))
        (map #(str/split-lines %))
        ((fn [pairs] (for [pair pairs] (map #(eval (read-string %)) pair))))
        (compare-packet-data)
        (map-indexed vector)
        (filter (fn [x] (true? (second x))))
        (map first)
        (map inc)
        (reduce +))
   ;; part 2: can use sort with our process-packet-pair
   (->> (slurp "src/advent/inputs/input13.txt")
        (#(str/split % #"\n\n"))
        (map #(str/split-lines %))
        ((fn [pairs] (for [pair pairs] (map #(eval (read-string %)) pair))))
        (apply concat)
        (concat [[[2]] [[6]]])
        (sort process-packet-pair)
        (map-indexed vector)
        (filter (fn [x] (or (= (second x) [[2]]) (= (second x) [[6]]))))
        (map first)
        (map inc)
        (reduce *))])

(comment (solve-day13))
