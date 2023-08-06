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

(defn solve-day10 []
  (let [input (->> (slurp "src/advent/inputs/input10.txt")
                   (str/split-lines))
        reg-x (execute-device-asm input)
        clock (mapv #(+ % 1) (vec (range (count reg-x))))
        power (mapv * reg-x clock)]
    (reduce + (mapv power [19 59 99 139 179 219]))))

(comment (solve-day10)
         (re-find #"addx" "noop"))
