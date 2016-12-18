(ns advent-of-code.2016.16)

(def initial-state [0 1 0 0 0 1 0 0 0 1 0 0 1 0 1 1 1])

(defn dragon-step [a]
  (let [b (reverse a)
        b' (vec (map #(- 1 %) b))
        ]
    (concat a [0] b')
  ))

(defn fill-disk [seed limit]
  (let [dragons (iterate dragon-step seed)
        long-enough (first (filter #(>= (count %) limit) dragons))
        ]
    (take limit long-enough)
    )
  )

(defn map-pair [p]
  (if (or (= '(1 1) p) (= '(0 0) p))
    1
    0
    )
  )

(defn checksum [s]
  (let [pairs (partition 2 s)
        check (map map-pair pairs)
        ]
    (if (even? (count check))
      (checksum check)
      check
      )
    )
  )

(defn solve1 []
  (let [limit 272
        seed initial-state
        fill (fill-disk seed limit)
        check (checksum fill)
        ]
    (println (apply str check))
  ))

(defn solve2 []
  (let [limit 35651584
        seed initial-state
        fill (fill-disk seed limit)
        check (checksum fill)
        ]
    (println (apply str check))
  ))

