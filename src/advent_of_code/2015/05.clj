(ns advent-of-code.2015.05)

(defn read-data []
  (with-open [rdr (clojure.java.io/reader "resources/2015/day05.txt")]
    (doall (line-seq rdr))))

(defn vowel? [c]
  (case c
    \a true
    \e true
    \i true
    \o true
    \u true
    false
    ))

(defn pair-equal? [[a b]]
  (= a b))

(defn all-pairs [s]
  (map list s (rest s)))

(defn bad-string? [s]
  (or (.contains s "ab")
      (.contains s "cd")
      (.contains s "pq")
      (.contains s "xy")
      ))

(defn good-string1? [s]
  (let [three-vowels (>= (count (filter vowel? s)) 3)
        has-pair (not (empty? (filter pair-equal? (all-pairs s))))
        ]
    (and three-vowels has-pair (not (bad-string? s)))
  ))

(defn solve1 []
  (let [content (read-data)]
    (println (count (filter good-string1? content)))))


;;;;;;;;;;;;;;;;;;;; day 2

(defn all-pairs-idx [s]
  (map list s (rest s) (range)))


;; Big doubt about this thing, maybe loop/recur would be better here
;; but I'm not sure how to iterate over a collection while having
;; a state with loop/recur
(defn repeating-pair? [s]
  (let [pairs (all-pairs-idx s)
        reducer (fn [[seen-pairs result] [a b idx]]
                  (if (contains? seen-pairs [a b])
                    (if (> idx (inc (seen-pairs [a b])))
                       [seen-pairs true]
                       [seen-pairs result])
                    [(assoc seen-pairs [a b] idx) result]
                    )
                  )
        [_ res] (reduce reducer [{} false] pairs)
        ]
    res
    ))

(defn intercalated-pair? [s]
  (let [tuples (map list s (rest s) (drop 2 s))
        filter-fn (fn [[a _ c]] (= a c))]
    (boolean (not-empty (filter filter-fn tuples)))
    ))

(defn good-string2? [s]
  (and (repeating-pair? s) (intercalated-pair? s)))

(defn solve2 []
  (let [content (read-data)]
    (println (count (filter good-string2? content)))
    ))
