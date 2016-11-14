(ns advent-of-code.2015.05)

;; (defn read-data [] (slurp "resources/2015/day05.txt"))
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

(defn solve2 [])
