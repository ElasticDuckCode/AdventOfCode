(ns advent.day11
  (:require [clojure.string :as str]))

(defn parse-monkeys
  ([lines] (parse-monkeys lines []))
  ([[line & lines] monkeys]
   (if (nil? line)
     monkeys
     (recur lines (cond (re-find #"Monkey" line)
                        (->> line
                             (#(str/split % #"\s+"))
                             (last)
                             (#(str/replace % #"[^0-9]" ""))
                             (#(read-string %))
                             (hash-map :inspects 0 :id)
                             (conj monkeys))

                        (re-find #"items" line)
                        (->> line
                             (#(str/split % #":"))
                             (last)
                             (#(str/split % #","))
                             (map str/trim)
                             (map #(read-string %))
                             (assoc (last monkeys) :items)
                             (conj (vec (butlast monkeys))))

                        (re-find #"Operation" line)
                        (->> line
                             (#(str/split % #":"))
                             (last)
                             (str/trim)
                             (#(str/split % #"\s+"))
                             (drop 2) ;; assuming always 3 terms, fixed operators
                             (#(fn [x]
                                 (let [left (str (first %))
                                       left (if (re-find #"old" left)
                                              x
                                              (read-string left))
                                       right (str (last %))
                                       right (if (re-find #"old" right)
                                               x
                                               (read-string right))
                                       op (str (nth % 1))]
                                   (case op
                                     "*" (* left right)
                                     "+" (+ left right)))))
                             (assoc (last monkeys) :op)
                             (conj (vec (butlast monkeys))))

                        (re-find #"Test" line) ;; alawys assuming divisible
                        (->> line
                             (#(str/split % #"\s+"))
                             (last)
                             (str/trim)
                             (read-string)
                             (#(fn [x] (= (mod x %) 0)))
                             (assoc (last monkeys) :test)
                             (conj (vec (butlast monkeys))))

                        (re-find #"If true" line)
                        (->> line
                             (#(str/split % #"\s+"))
                             (last)
                             (str/trim)
                             (read-string)
                             (assoc (last monkeys) :true-target)
                             (conj (vec (butlast monkeys))))

                        (re-find #"If false" line)
                        (->> line
                             (#(str/split % #"\s+"))
                             (last)
                             (str/trim)
                             (read-string)
                             (assoc (last monkeys) :false-target)
                             (conj (vec (butlast monkeys))))

                        :else monkeys)))))

(defn play-monkey-in-the-middle [monkeys])

(defn solve-day11 []
  (let [monkeys
        (->> (slurp "src/advent/inputs/input11.txt")
             (str/split-lines)
             (map str/trim)
             (parse-monkeys))]
    monkeys))

(comment (solve-day11)
         ((get (nth (solve-day11) 1) :op) 10))
