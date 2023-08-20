(ns advent.day14
  (:require
   [clojure.string :as str]
   [clojure.set :refer [union]]))

(defn parse-pair [pair]
  (map read-string (str/split pair #",")))

(defn two-index-range [[x1 y1] [x2 y2]]
  (cond
    (> x1 x2) (two-index-range [x2 y1] [x1 y2])
    (> y1 y2) (two-index-range [x1 y2] [x2 y1])
    :else (apply concat
                 (for [x (range x1 (inc x2))]
                   (for [y (range y1 (inc y2))]
                     [x y])))))

(defn parse-wall-locations
  ([points] (parse-wall-locations points []))

  ([[p1 p2 & rest] loc]
   (if-not (empty? rest)
     (recur (cons p2 rest) (concat loc (two-index-range p1 p2)))
     (set (concat loc (two-index-range p1 p2))))))

(defn find-minx [loc]
  (reduce min (map first loc)))

(defn find-miny [loc]
  (reduce min (map second loc)))

(defn find-minxy [loc]
  [(find-minx loc) (find-miny loc)])

(defn print-wall-locations
  ([point sand-loc loc]
   (let [lx (reduce min (map first loc))
         hx (reduce max (map first loc))
         ly 0 ;; always want to start at 0 for height
         hy (reduce max (map second loc))]
     (print (apply str
                   (for [j (range ly (+ hy 2))]
                     (str (apply str (for [i (range (- lx 9) (+ hx 10))]
                                       (cond
                                         (contains? loc [i j]) "#"
                                         (contains? sand-loc [i j]) "o"
                                         (= point [i j]) "+"
                                         :else "."))) "\n"))))))
  ([sand-loc loc]
   (let [lx (reduce min (map first loc))
         hx (reduce max (map first loc))
         ly 0 ;; always want to start at 0 for height
         hy (reduce max (map second loc))]
     (print (apply str
                   (for [j (range ly (+ hy 2))]
                     (str (apply str (for [i (range (- lx 1) (+ hx 2))]
                                       (cond
                                         (contains? loc [i j]) "#"
                                         (contains? sand-loc [i j]) "o"
                                         :else "."))) "\n")))))))

(defn is-sand-at-rest? [[x y] sand-loc loc]
  (and
   (or (contains? sand-loc [x (inc y)])
       (contains? loc [x (inc y)]))
   (or (contains? sand-loc [(dec x) (inc y)])
       (contains? loc [(dec x) (inc y)]))
   (or (contains? sand-loc [(inc x) (inc y)])
       (contains? loc [(inc x) (inc y)]))))

(defn drop-sand [[x y] sand-loc loc]

  ; (print-wall-locations [x y] sand-loc loc)
  ; (println)
  ; (flush)
  ; (Thread/sleep 10)

  (if (>= y (+ 1 (reduce max (map second loc))))
    [x y]
    (if-not (or (contains? sand-loc [x (inc y)])
                (contains? loc [x (inc y)]))
      (recur [x (inc y)] sand-loc loc)
      (if-not (or (contains? sand-loc [(dec x) (inc y)])
                  (contains? loc [(dec x) (inc y)]))
        (recur [(dec x) (inc y)] sand-loc loc)
        (if-not (or (contains? sand-loc [(inc x) (inc y)])
                    (contains? loc [(inc x) (inc y)]))
          (recur [(inc x) (inc y)] sand-loc loc)
          [x y])))))

(defn simulate-sand-bottomless [source sand-loc loc]
  (let [[x y] (drop-sand source sand-loc loc)]
    (if (>= y (reduce max (map second loc)))
      sand-loc
      (recur source (conj sand-loc [x y]) loc))))

(defn simulate-sand-bottom [source sand-loc loc]
  (let [[x y] (drop-sand source sand-loc loc)]
    (if (contains? sand-loc [x y])
      sand-loc
      (do
        (println (count sand-loc))
        (recur source (conj sand-loc [x y]) loc)))))

(defn solve-day14 []
  (let [loc (->> "src/advent/inputs/input14.txt"
                 (slurp)
                 (str/split-lines)
                 (map #(map parse-pair (str/split % #" -> ")))
                 (map parse-wall-locations)
                 (apply union))]
    ;(count (simulate-sand-bottomless [500 0] #{} loc))
    (count (simulate-sand-bottom [500 0] #{} loc))))

(comment (solve-day14))

