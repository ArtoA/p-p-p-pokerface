(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit  fst)
      (Integer/valueOf (str fst))
      (replacements fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(1 1 1 2)))

(defn three-of-a-kind? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(1 1 3)))

(defn four-of-a-kind? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(1 4)))

(defn flush? [hand]
  (= (vals (frequencies (map suit hand))) '(5)))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(2 3)))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand)))) '(1 2 2)))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        sorted-ranks-low-ace (sort (replace {14, 1} sorted-ranks))
        test (fn [ranks] (= ranks (range (first ranks) (+ (last ranks) 1) )))]
    (or (test sorted-ranks)
        (test sorted-ranks-low-ace))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
(let [checkers #{[high-card? 0] [pair? 1] [two-pairs? 2]
                 [three-of-a-kind? 3] [straight? 4] [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7] [straight-flush? 8]}
      test-hand  (fn [checker] ((first checker) hand))]
  (apply max (map second (filter test-hand checkers)))))
