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

(defn monkey-pop-item [monkey]
  [(first (get monkey :items))
   (assoc monkey :items (rest (get monkey :items)))])

(defn monkey-push-item [item monkey]
  (assoc monkey :items (reverse (conj (reverse (get monkey :items)) item))))

(defn monkey-increase-inspects [monkey]
  (assoc monkey :inspects (inc (get monkey :inspects))))

(defn monkey-inspect-item [monkey]
  (let [[item monkey] (monkey-pop-item monkey)
        inspect (get monkey :op)]
    (if-not (nil? item)
      [(-> item (inspect) (quot 3)) (monkey-increase-inspects monkey)]
      [nil monkey])))

(defn monkey-throw-item [item monkey monkeys]
  (if-not (nil? item)
    (let [testfn (get monkey :test)
          target (if (testfn item)
                   (get monkey :true-target)
                   (get monkey :false-target))]
      (assoc monkeys target (monkey-push-item item (nth monkeys target))))
    monkeys))

(defn monkeys-get-inspects [monkeys]
  (for [monkey monkeys]
    (get monkey :inspects)))

(defn get-monkey-buisness [monkeys]
  (reduce * (take 2 (sort > (monkeys-get-inspects monkeys)))))

(defn play-monkey-turn [k monkeys]
  (if (< k (count monkeys))
    (let [monkey (nth monkeys k)
          [item monkey] (monkey-inspect-item monkey)
          monkeys (assoc monkeys k monkey)]
      (if-not (nil? item)
        (recur k (monkey-throw-item item monkey monkeys))
        (recur (inc k) monkeys)))
    monkeys))

(defn play-monkey-in-the-middle
  ([monkeys]
   (play-monkey-turn 0 monkeys))

  ([monkeys n]
   (if (pos? n)
     (recur
      (play-monkey-in-the-middle monkeys)
      (dec n))
     monkeys)))

(defn solve-day11 []
  (let [monkeys
        (->> (slurp "src/advent/inputs/input11.txt")
             (str/split-lines)
             (map str/trim)
             (parse-monkeys))
        monkeys (play-monkey-in-the-middle monkeys 20)]
    (get-monkey-buisness monkeys)))

(comment (solve-day11)
         ((get (nth (solve-day11) 1) :op) 10))
