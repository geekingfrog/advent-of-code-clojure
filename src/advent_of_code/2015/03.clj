(ns advent-of-code.2015.03)

(defn read-data []
  (slurp "resources/2015/day03.txt"))

(defn drop-present [[[x y] visited-houses] c]
  (let [new-pos (case c
                  \^ [x (+ y 1)]
                  \v [x (- y 1)]
                  \> [(+ x 1) y]
                  \< [(- x 1) y])
        updated-houses (conj visited-houses new-pos)]
    (identity [new-pos updated-houses])
    ))

(defn solve1 []
  (let [content (read-data)
        initial-state [[0 0] #{[0 0]}]
        [final-pos final-houses] (reduce drop-present initial-state content)]
    (println (count final-houses)))
  )

(defn solve2 []
  (let [content (read-data)
        santa-list (take-nth 2 content)
        robot-list (take-nth 2 (rest content))
        initial-state [[0 0] #{[0 0]}]
        [_ santa-houses] (reduce drop-present initial-state santa-list)
        [_ robot-houses] (reduce drop-present initial-state robot-list)
        ]
    (println (count (apply conj santa-houses robot-houses)))
  ))
