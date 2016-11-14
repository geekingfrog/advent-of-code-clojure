(ns advent-of-code.2015.01)

(defn read-file []
  (slurp "resources/2015/day01.txt"))

(defn track-floors [acc c]
  (let [current-floor (first acc)
       idx (second acc)
        next-idx (+ idx 1)]
    (if (= \( c)
      (identity [(+ current-floor 1) next-idx])
      (identity [(- current-floor 1) next-idx]))
  ))

(defn solve1 []
  (let [content (read-file)
        final-state (reduce track-floors [0 0] content)]
    ;; (println (reduce track-floors (identity [0 0]) content))
    (println (first final-state))
    ))

(defn solve2 []
  (let [content (read-file)
        all-states (reductions track-floors [0 0] content)
        filtered (filter #(= -1 (first %)) all-states)
        ]
    (println (second (first filtered)))
    )
  )
