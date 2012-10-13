(ns derri.core)

(defn roman-lookup [c] (get  {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}) c)

(defn roman-seq [s] (map roman-lookup s))

((fn [s] 
  (let [rrs (reverse (map #(get {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000} %) s))]
   (loop [coll rrs n (first coll)]
     (let [c (count coll) fc (first coll) sc (second coll)]
      (case c
       0  0
       1  n
       2  (if (> fc sc) (- n sc) (+ n sc))
       (recur (drop 1 coll) (if (> fc sc) (- n sc) (+ n sc)))))))) "XLVIII")

                      

      
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

    

