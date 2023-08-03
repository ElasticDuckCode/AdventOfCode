(ns advent.day7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn path-join [& paths]
  (str (apply io/file paths)))

(defn path-cd-prev [path]
  (let [path (apply path-join (butlast (str/split path, #"/")))]
    (if (= path "")
      "/"
      path)))

(defn process-input [input]
  (mapv #(str/split % #"\s+") (str/split-lines input)))

(defn process-cmd [pwd cmd]
  (case (first cmd)
    "cd" (case (nth cmd 1)
           "/", "/"
           "..", (if (= pwd "/") pwd (path-cd-prev pwd))
           (path-join pwd (nth cmd 1)))
    pwd)) ;;ignoring ls

(defn process-dir [pwd dirs dirname]
  (let [dirpath (path-join pwd dirname)]
    (assoc dirs dirpath 0)))

(defn process-file [pwd dirs fname fsize]
  (let [dirs (assoc dirs pwd (+ fsize (get dirs pwd)))]
    (if (= pwd "/")
      dirs
      (process-file (path-cd-prev pwd) dirs fname fsize))))

(defn get-directory-sizes [pwd dirs [cmd & cmds]]
  (case (first cmd)
    nil dirs
    "$" (let [pwd (process-cmd pwd (rest cmd))]
          (get-directory-sizes pwd dirs cmds))
    "dir" (let [dirs (process-dir pwd dirs (nth cmd 1))]
            (get-directory-sizes pwd dirs cmds))
    (let [dirs (process-file pwd dirs (nth cmd 1) (Integer/parseInt (nth cmd 0)))]
      (get-directory-sizes pwd dirs cmds))))

(defn solve-day7 []
  (let [input (slurp "src/advent/inputs/input7.txt")
        files (->> input
                   (process-input)
                   (get-directory-sizes "/" {"/" 0}))
        total-space 70000000
        needed-free 30000000
        used-space (get files "/")
        free-space (- total-space used-space)
        delete-at-least (- needed-free free-space)]
    (->> files ;; part 1
         (vals)
         (filter (fn [x] (<= x 100000)))
         (reduce +)
         (println))
    (->> files
         (vals)
         (filter (fn [x] (>= x delete-at-least)))
         (#(apply min %))
         (println))))

(comment (reduce + (filter (fn [x] (<= x 100000)) (vals (solve-day7))))
         (= "/" "/")
         (process-file "/apple" {"/" 0 "/apple" 0} "bingus" 32840)
         (first nil)
         (apply path-join (butlast (str/split "/apple/pan", #"/")))
         (path-cd-prev "/")
         (path-cd-prev "/apple")
         (keyword "/apple")
         (let [my-map {"/" 0 "/apple" 5 "/pear" 10}]
           (filter
            #(str/starts-with? % "/apple")
            (keys (assoc my-map "/apple/bottom" 0)))))
