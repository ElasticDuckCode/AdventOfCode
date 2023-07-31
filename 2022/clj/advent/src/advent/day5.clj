(ns advent.day5
  (:require
   [clojure.string :as str]))

(defn get-crates [input]
  (first (str/split input #"\n\n")))

(defn get-instructions [input]
  (last (str/split input #"\n\n")))

(defn split-whitespace [input]
  (str/split (str/trim input) #"\s+"))

(defn num-crates [input]
  (->> input
       (get-crates)
       (str/split-lines)
       (last)
       (split-whitespace)
       (mapv #(Integer/parseInt %))
       (last)))

(defn build-ith-stack [input, i]
  (let [crates (->> input
                    (get-crates)
                    (str/split-lines)
                    (reverse)
                    (rest)
                    (reverse))]
    (apply list (filterv (fn [x] (not= \space x)) (mapv (fn [row] (get row (+ (* i 4) 1))) crates)))))

(defn process-crates [input]
  (let [n (num-crates input)]
    (mapv #(build-ith-stack input %) (range n))))

(defn process-instructions [input]
  (let [instructions (get-instructions input)]
    (mapv
     (fn [x] (mapv #(Integer/parseInt %) (mapv (split-whitespace x) [1 3 5])))
     (str/split-lines instructions))))

(defn apply-instruction [crates num from to]
  (assoc (assoc crates
                (- to 1)
                (apply conj
                       (get crates (- to 1))
                       (take num (get crates (- from 1)))))
         (- from 1)
         (drop num (get crates (- from 1)))))

(defn apply-instruction-reversed [crates num from to]
  (assoc (assoc crates
                (- to 1)
                (concat
                 (take num (get crates (- from 1)))
                 (get crates (- to 1))))
         (- from 1)
         (drop num (get crates (- from 1)))))

(defn apply-instructions [crates [[num from to] & instructions]]
  (let [new-crates (apply-instruction crates num from to)]
    (if instructions
      (apply-instructions new-crates instructions)
      new-crates)))

(defn apply-instructions-reversed [crates [[num from to] & instructions]]
  (let [new-crates (apply-instruction-reversed crates num from to)]
    (if instructions
      (apply-instructions-reversed new-crates instructions)
      new-crates)))

(defn get-tops [crates]
  (mapv #(first (list* %)) crates))

(defn solve-day5 []
  (let [input (slurp "src/advent/inputs/input5.txt")
        crates (process-crates input)
        instructions (process-instructions input)
        new-crates (apply-instructions crates instructions)
        new-crates-reversed (apply-instructions-reversed crates instructions)]
    (println new-crates)
    (println (get-tops new-crates))
    (println new-crates-reversed)
    (println (get-tops new-crates-reversed))))
