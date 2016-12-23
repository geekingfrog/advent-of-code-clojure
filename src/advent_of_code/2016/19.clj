(ns advent-of-code.2016.19)


;; (def input 5)
(def input 5)

(defn steal [n elves i]
  "elf #i steals stuff
  return [stolen? new-gift-distribution]"
  (let
    [idxs (map #(mod %1 n) (range (inc i) (+ i n)))
     valid-idxs (filter #(not= 0 (nth elves %1)) idxs)
     ]
    (if (empty? valid-idxs)
      [false elves]
      (let [target (first valid-idxs)
            stolen-gifts (nth elves target)
            new-gifts (+ stolen-gifts (nth elves i))]
        [true (assoc elves i new-gifts target 0)]
        )
      )))

(defn steal-gifts [n]
  (loop [elves (vec (replicate n 1)) i 0]
    (if (= 0 (nth elves i))
      (recur elves (mod (inc i) n))
      (let [[stolen? elves'] (steal n elves i)]
        (if (not stolen?)
          elves
          (recur elves' (mod (inc i) n))
        ))
      )
    )
  )

(defn find-idx [elves]
  (loop [i 0]
    (if (not= 0 (nth elves i))
      i
      (recur (inc i)))))

(defn solve1 []
  (->>
    (steal-gifts 3018458)
    (find-idx)
    (inc)
    (println)
    ))


(defn delete-elem [xs i]
  (vec (concat
         (subvec xs 0 i)
         (subvec xs (inc i))
         ))
  )

(defn steal' [elves i]
  "elf #i steals stuff
  return [stolen? new-gift-distribution]"
  (let [n (count elves)]
    (if (= 1 n)
      (first elves)
      (let [[elf-idx elf-gifts] (nth elves i)
            mid (int (/ n 2))
            target (mod (+ i mid) n)
            stolen-gifts (second (nth elves target))
            new-gifts (+ stolen-gifts elf-gifts)
            new-elves (assoc elves i [elf-idx new-gifts])
            ]
        (println n " removing elf in position " (first (nth elves target)))
        (delete-elem new-elves target)
        )
      )
    )
  )

(defn steal-gifts' [n]
  (loop [elves (vec (map vector (range 1 (inc n)) (replicate n 1))) i 0]
    ;; (println elves " - " (counted? elves))
    (if (= 1 (count elves))
      (first elves)
      (recur (steal' elves i) (mod (inc i) (count elves)))
      )
    )
  )
    ;; (println "starting elves: " elves)
    ;; (steal' n elves 0)))

(defn solve2 [] (println (steal-gifts' 3018458)))
;; (defn solve2 [] (println (steal-gifts' 5)))
