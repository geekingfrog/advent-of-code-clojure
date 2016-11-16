(ns advent-of-code.2015.06
  (:require [instaparse.core :as insta]))

(def limit 1000)

(def grammar "
  instruction = instruction-type ' ' square ' through ' square
  instruction-type = 'turn on' | 'turn off' | 'toggle'
  square = number ',' number
  number = ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9')+
  ")

(def line-parser (insta/parser grammer))

(defn parse-line [l]
  (let [transform-map {:number (fn [& digits] (read-string (apply str digits)))}]
    (insta/transform transform-map (line-parser l))
  ))

(defn inst-fn1 [inst-type grid n]
  (case inst-type
    "turn on" 1
    "turn off" 0
    "toggle" (- 1 (grid n))
    (println (str "No match for instruction type " inst-type))
    )
  )

(defn inst-fn2 [inst-type grid n]
  (case inst-type
    "turn on" (inc (grid n))
    "turn off" (max (dec (grid n)) 0)
    "toggle" (+ 2 (grid n))
   )
  )

(defn apply-instruction [update-fn grid instruction]
  (let [[_ [_ inst-type] _ bl _ tr] instruction
        [_ blX _ blY] bl
        [_ trX _ trY] tr
        updated-vals (for [x (range blX (inc trX))
                           y (range blY (inc trY))
                           :let [pos (+ (* x limit) y)]]
                       [pos (update-fn inst-type grid pos)])
        ]
    (apply assoc grid (apply concat updated-vals))
   ))

(defn parse-data []
  (with-open [rdr (clojure.java.io/reader "resources/2015/day06.txt")]
    (doall (map parse-line (line-seq rdr)))
    )
  )


(defn solve1 []
  (let [grid (apply vector (repeat (* limit limit) 0))
        instructions (parse-data)
        final-grid (reduce (partial apply-instruction inst-fn1) grid instructions)
        ]
    (println (count (filter #(= 1 %) final-grid)))
  ))

(defn solve2 []
  (let [grid (apply vector (repeat (* limit limit) 0))
       instructions (parse-data)
       final-grid (reduce (partial apply-instruction inst-fn2) grid instructions)
       ]
  (println (reduce + final-grid))
  ))
