(ns advent.day6)

(defn find-marker [len pos data-stream]
  (if (nil? data-stream)
    -1
    (if (apply distinct? (take len data-stream))
      (+ len pos)
      (find-marker len (+ pos 1) (drop 1 data-stream)))))

(defn solve-day6 []
  (let [input (slurp "src/advent/inputs/input6.txt")]
    (println (find-marker 4 0 input))
    (println (find-marker 14 0 input))))
