(ns advent.day10
  (:require [clojure.string :as str]))

(defn execute-device-asm
  ([instrs]
   (execute-device-asm instrs [1]))
  ([[instr & instrs] x]
   (cond
     (nil? instr) x
     (re-find #"noop" instr) (execute-device-asm instrs (conj x (last x)))
     (re-find #"addx" instr) (let [v (Integer/parseInt (last (str/split instr #"\s+")))]
                               (execute-device-asm instrs (conj x (last x) (+ (last x) v)))))))

(defn draw-crt
  ([clock x] (draw-crt clock x ""))
  ([clock x screen]
   (if (or (empty? clock) (empty? x))
     screen
     (let [horz (mod (first clock) 40)
           screen (if (zero? horz) (str screen "\n") screen)
           diff (abs (- horz (first x)))]
       (cond
         (<= diff 1) (recur (rest clock) (rest x) (str screen "#"))
         :else (recur (rest clock) (rest x) (str screen ".")))))))

(defn solve-day10 []
  (let [input (->> (slurp "src/advent/inputs/input10.txt")
                   (str/split-lines))
        reg-x (execute-device-asm input)
        clock (vec (range (count reg-x)))
        power (mapv * reg-x (mapv #(+ % 1) clock))]
    (println (reduce + (mapv power [19 59 99 139 179 219])))
    (println (draw-crt clock reg-x))))

(comment (solve-day10)
         (re-find #"addx" "noop")
         (draw-crt (range 10) (take 10 (repeatedly #(rand-int 64)))))
