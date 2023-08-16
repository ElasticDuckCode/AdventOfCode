(ns advent.matrix)

(defn get-row [matrix, m]
  (nth matrix m))

(defn get-col [matrix, n]
  (mapv #(nth % n) matrix))

(defn get-element [matrix, m, n]
  (get (get matrix m) n))

(defn size
  ([matrix]
   (let [n-row (count matrix)
         n-col (count (get matrix 0))]
     (vector n-row n-col)))
  ([matrix dim]
   (let [n-row (count matrix)
         n-col (count (get matrix 0))]
     (case dim
       0 n-row
       1 n-col))))

(defn zeros [[m, n]]
  (vec (repeat m (vec (repeat n 0)))))

(defn infs [[m, n]]
  (vec (repeat m (vec (repeat n ##Inf)))))

(defn remove-at [v ith]
  (into (subvec v 0 ith) (subvec v (inc ith))))

(defn reduce-matrix [f matrix]
  (reduce f (mapv #(reduce f %) matrix)))

(defn sign [n]
  (let [sign-fn (fn [x] (if (neg? x) -1 (if (pos-int? x) 1 0)))]
    (if (coll? n)
      (mapv #(sign %) n)
      (sign-fn n))))
