(ns advent.day7
  (:require [clojure.string :as str]))

(defn process-input [input]
  (map #(str/split % #"\s+") (str/split-lines input)))

(defn process-cmd [pwd cmd]
  (case (first cmd)
    "cd" cmd
    :default))

(defn get-directory-sizes [pwd dirs [cmd & cmds]]
  (case (first cmd)
    "$" (process-cmd pwd (rest cmd))
    "dir"
    "default"))

(defn solve-day7 []
  (let [input (slurp "src/advent/inputs/input7.txt")]
    (get-directory-sizes "/" {"/" 0} (process-input input))))

(comment (solve-day7)
         (keyword "/apple")
         (let [my-map {"/" 0 "/apple" 5 "/pear" 10}]
           (filter
            #(str/starts-with? % "/apple")
            (keys (assoc my-map "/apple/bottom" 0)))))
