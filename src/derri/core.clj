(ns derri.core)

;;(defn roman-lookup [c] (get  {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}) c)

;;(defn roman-seq [s] (map roman-lookup s))

(fn [s]
  ;; reverse a list of translated romans "XIV" -> (10 1 5) -> (5 1 10)
  (let [rrs (reverse (map #(get {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %) s))]
   ;; loop-recur 
   (loop [coll rrs n (first coll)]
     (let [c (count coll) fc (first coll) sc (second coll)
          ;; if first number is > second number, subtract second number from total
          ;;   else, add second number to total
          ;;  (5 1 10) -->  5 > 1 -->  5 - 1 = 4
          ;;  on recur...
          ;;     (1 10) --> 1 <= 10 --> 4 + 10 = 14
           next-add-or-sub (if (> fc sc) (- n sc) (+ n sc))]
      (case c
       0  0
       1  n
       2  next-add-or-sub
        ;; default 
       (recur (drop 1 coll) next-add-or-sub ))))))

                      

;; Game of Life
(fn [xcoll]
  ;; reconstructs flattened vec (\space \space \space \space \# \# ...) --> ("    ##" ...)
  (map #(reduce str %) (partition (count (first xcoll))

   (map #(let [cell (nth (reduce str xcoll) %)  ;; cell = current element
              n ((fn [coll i]                   ;; 'get-neighbors' inline function
    (let [w (count (first coll))                ;; w = width
         h (count coll)                         ;; h = height
         x (mod i w)                            ;; x y coords
         y (int (/ i w))
         life \#]                               ;; life
   ;; sum up neighbors
     (reduce + (list (if (> x 0) (if (= (nth (nth coll y) (dec x)) life) 1 0) 0)                     
                     (if (> y 0) (if (= (nth (nth coll (dec y)) x) life) 1 0) 0)
                     (if (< x (dec w)) (if (= (nth (nth coll y) (inc x)) life) 1 0) 0) 
                     (if (< y (dec h)) (if (= (nth (nth coll (inc y)) x) life) 1 0) 0)
                     (if (and (> x 0) (> y 0))       (if (= (nth (nth coll (dec y)) (dec x)) life) 1 0) 0)
                     (if (and (> x 0) (< y (dec h))) (if (= (nth (nth coll (inc y)) (dec x)) life) 1 0) 0)
                     (if (and (< x (dec w)) (> y 0)) (if (= (nth (nth coll (dec y)) (inc x)) life) 1 0) 0)
                     (if (and (< x (dec w)) (< y (dec h))) (if (= (nth (nth coll (inc y)) (inc x)) life) 1 0) 0))))) xcoll %)]           (if (= cell \#)
            (case n            ;; evaluate living cells
              (0 1) " "        ;;  0 or 1 --> dead
              (2 3) \#         ;;  2 or 3 --> alive
              " ")             ;;  > 3    --> dead
            (if (= n 3) \# " ")))  ;; dead cells with 3 neighbors come alive
       (range 0 (* (count (first xcoll)) (count xcoll)))))))  ; range to map over indexes