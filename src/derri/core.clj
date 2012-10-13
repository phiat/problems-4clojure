(ns derri.core)

(defn roman-lookup [c] (get  {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}) c)

(defn roman-seq [s] (map roman-lookup s))

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

                      

      
(def  t1 [      "      " 
                " ##   "
                " ##   "
                "   ## "
                "   ## "
                "      "])
(def t2 [       "      " 
                " ##   "
                " #    "
                "    # "
                "   ## "
                "      "])

(defn test-life [a b] ())

    

