;; best hand
;;  poker best hand parser

;; (= :high-card (__ ["HA" "D2" "H3" "C9" "DJ"]))
;; (= :pair (__ ["HA" "HQ" "SJ" "DA" "HT"]))
;; (= :two-pair (__ ["HA" "DA" "HQ" "SQ" "HT"]))
;; (= :three-of-a-kind (__ ["HA" "DA" "CA" "HJ" "HT"]))
;; (= :straight (__ ["HA" "DK" "HQ" "HJ" "HT"]))
;; (= :straight (__ ["HA" "H2" "S3" "D4" "C5"]))
;; (= :flush (__ ["HA" "HK" "H2" "H4" "HT"]))
;; (= :full-house (__ ["HA" "DA" "CA" "HJ" "DJ"]))
;; (= :four-of-a-kind (__ ["HA" "DA" "CA" "SA" "DJ"]))
;; (= :straight-flush (__ ["HA" "HK" "HQ" "HJ" "HT"]))



(defn suit [c] (first c))
(defn rank [c] (last c))

(def ranks [2 3 4 5 6 7 8 9 \T \J \Q \K \A])

(.indexOf ranks \A)

;; straight flush  (dont need :) )
(defn straight-flush? [h]
  (every? #(= (suit (first h)) %) (map suit h)))

(def hand ["HA" "HK" "HQ" "HJ" "HT"])
(straight-flush? hand)

;; four-of-a-kind
(defn four-of-a-kind? [h]
  (if (some #(= 4 %) (vals (frequencies (map rank h)))) true false))

(def hand2 ["HA" "DA" "CA" "SA" "DJ"])

(four-of-a-kind? hand2)


;;fullhouse   (remember to check before two-pair)
;;pair
;;three of a kind
;;two-pair

(defn pair? [h]
  (if (some #(= 2 %) (vals (frequencies (map rank h)))) true false))

(defn two-pair? [h]
  (if (= 3 (count (frequencies (map rank h)))) true false))


(defn trips? [h]
  (if (some #(= 3 %) (vals (frequencies (map rank h)))) true false))

(defn fullhouse? [h]
  (and (pair? h) (trips? h)))


(def hand3 ["HA" "HQ" "SJ" "DA" "HT"])
(def hand4 ["HA" "DA" "CA" "H3" "HT"])
(def hand5 ["HA" "DA" "CA" "HJ" "DJ"])
(def hand6 ["HA" "DA" "HQ" "SQ" "HT"])
(pair? hand3)
(trips? hand4)
(fullhouse? hand5)
(two-pair? hand2)

;;straight
(defn straight? [h]
  (let [indexed-ranks (sort (map #(.indexOf ranks %) (map rank h)))
        ends-diff (- (first indexed-ranks) (last indexed-ranks))]
    (cond
     (not= 5 (count (vals (frequencies indexed-ranks)))) false
     (= ends-diff -4) true
     (= indexed-ranks '(2 3 4 5 12)) true
     :else false)))

(def hand7 ["HA" "DK" "HQ" "HJ" "HT"])

(straight? hand7)
(straight? ["HA" "DA" "DQ" "QJ" "HT"])

;; flush
(defn flush? [h]
  (= 1 (count (vals (frequencies (map suit h))))))

(flush? ["HA" "HK" "H2" "H4" "HT"])

(defn best-hand [h]
  (cond
   (straight-flush? h) :straight-flush
   (four-of-a-kind? h) :four-of-a-kind
   (fullhouse? h)     :full-house
   (flush? h)          :flush
   (straight? h)       :straight
   (trips? h) :three-of-a-kind
   (two-pair? h)       :two-pair
   (pair? h)           :pair
   :else               :high-card   ))

(def hands [hand hand2 hand3 hand4 hand5 hand6 hand7])

(map best-hand hands)



;; the big mess


(defn best-hand [hand]
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

(best-hand ["DA" "DK" "DQ" "DJ" "DT"])
(best-hand ["H4" "D4" "C4" "HJ" "HT"])

;; notes
;;   frequencies, max-key
