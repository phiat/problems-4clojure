(ns problems-4clojure.core)

;; solutions to 'hard' problems at http://www.4clojure.com/

;; Read Roman numerals
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
     (reduce + (list (if (> x 0) (if (= (nth (nth coll y) (dec x)) life) 1 0) 0)            ;; x - 1 , y          
                     (if (> y 0) (if (= (nth (nth coll (dec y)) x) life) 1 0) 0)            ;; x     , y - 1  
                     (if (< x (dec w)) (if (= (nth (nth coll y) (inc x)) life) 1 0) 0)      ;; x + 1 , y 
                     (if (< y (dec h)) (if (= (nth (nth coll (inc y)) x) life) 1 0) 0)      ;; x     , y + 1
                                                                                            ;; diagonals
                     (if (and (> x 0) (> y 0))       (if (= (nth (nth coll (dec y)) (dec x)) life) 1 0) 0)
                     (if (and (> x 0) (< y (dec h))) (if (= (nth (nth coll (inc y)) (dec x)) life) 1 0) 0)
                     (if (and (< x (dec w)) (> y 0)) (if (= (nth (nth coll (dec y)) (inc x)) life) 1 0) 0)
                     (if (and (< x (dec w)) (< y (dec h))) (if (= (nth (nth coll (inc y)) (inc x)) life) 1 0) 0))))) xcoll %)]           
           (if (= cell \#)
            (case n            ;; evaluate living cells
              (0 1) " "        ;;  0 or 1 --> dead
              (2 3) \#         ;;  2 or 3 --> alive
              " ")             ;;  > 3    --> dead
            (if (= n 3) \# " ")))                             ;; dead cells with 3 neighbors come alive
       (range 0 (* (count (first xcoll)) (count xcoll)))))))  ;; range to map over indexes



;; analyze tic-tac-toe
;;    return :x or :o if X or O wins, otherwise, nil
;;  (= nil (__ [[:x :o :x]    
;;              [:x :o :x] 
;;              [:o :x :o]]
;;  (= :x (__  [[:x :e :o]
;;              [:x :e :e]
;;              [:x :e :o]]))
;;
(fn [b] (let [r1 (first b)                          ;;rows
              r2 (second b) 
              r3 (last b)
              c1 (map first [r1 r2 r3])             ;;cols
              c2 (map second [r1 r2 r3])
              c3 (map last [r1 r2 r3])
              d1 [(first r1) (second r2) (last r3)] ;;diagonals
              d2 [(last r1) (second r2) (first r3)]
              tx (fn [r] (if (every? #(= % :x) r) :x nil))  ;; is the triplet all x's
              to (fn [r] (if (every? #(= % :o) r) :o nil))  ;; is the triplet all o's
              fa [  (tx r1) (tx r2) (tx r3)         ;; test each triplet
                    (to r1) (to r2) (to r3)         ;; will return [ nil nil :x :x nil ...]
                    (tx c1) (tx c2) (tx c3)
                    (to c1) (to c2) (to c3)
                    (tx d1) (tx d2)
                    (to d1) (to d2)]
              fxi (.indexOf fa :x)                 ;; returns > 0 if found
              foi (.indexOf fa :o)]
           (cond (> fxi -1) :x                     ;; test returns
                 (> foi -1) :o
                 :else nil)))
    
;; best poker hand   - see best-hand.clj
(fn [hand]
  (let [suit (fn [c] (first c))
        rank (fn [c] (last c))
        ranks [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A]
        four-of-a-kind? (fn [h] (if (some #(= 4 %) (vals (frequencies (map rank h)))) true false))
        flush?          (fn [h]   (= 1 (count (vals (frequencies (map suit h))))))
        straight?       (fn [h] (let [indexed-ranks (sort (map #(.indexOf ranks %) (map rank h)))
                                  ends-diff (- (first indexed-ranks) (last indexed-ranks))]
                              (cond
                               (and (= 5 (count (frequencies indexed-ranks)))
                               (= ends-diff -4)) true
                               (= indexed-ranks '(0 1 2 3 12)) true
                               :else false)))

        trips?       (fn [h]  (if (some #(= 3 %) (vals (frequencies (map rank h)))) true false))
        two-pair?    (fn [h](if (= 3 (count (frequencies (map rank h)))) true false))
        pair?        (fn [h](if (some #(= 2 %) (vals (frequencies (map rank h)))) true false))
        straight-flush? (fn [h] (and (straight? h) (flush? h)))
        full-house?     (fn [h] (= 2 (count (frequencies (map rank h) ))))
        ]



  (cond
   (straight-flush? hand) :straight-flush
   (four-of-a-kind? hand) :four-of-a-kind
   (full-house? hand)     :full-house
   (flush? hand)          :flush
   (straight? hand)       :straight
   (trips? hand) :three-of-a-kind
   (two-pair? hand)       :two-pair
   (pair? hand)           :pair
   :else               :high-card   )))
;; 

