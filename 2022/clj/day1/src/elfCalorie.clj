(ns elfCalorie
  (:require [clojure.string :as str]))

(def file-data (str/split (slurp "input.txt") #"\r?\n\n"))                      ;; split input data based on two newlines

(defn -main []
  (let [result
        (mapv #(reduce + (mapv read-string (str/split-lines %))) file-data)]    ;; split data again, but eval as 
                                                                                ;; integers and reduce the sum 

    (println (reduce max result))                                               ;; part 1: highest
    (println (reduce + (take 3 (sort > result))))))                             ;; part 2: sum of 3 highest
