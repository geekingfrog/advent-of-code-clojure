(ns advent-of-code.2016.21
  (:require [instaparse.core :as insta]))

(def grammar "
  <instructions> = instruction*
  <instruction> = (swap-pos
               | swap-letter
               | rotate
               | rotate-pos
               | reverse
               | move-pos
             ) <'\n'>
  swap-pos = <'swap position '> num <' with position '> num
  swap-letter = <'swap letter '> letter <' with letter '> letter
  rotate = <'rotate '> direction <' '> num <' step' | ' steps'>
  rotate-pos = <'rotate based on position of letter '> letter
  reverse = <'reverse positions '> num <' through '> num
  move-pos = <'move position '> num <' to position '> num
  direction = 'left' | 'right'
  num = #'[0-9]+'
  letter = #'[a-z]'
  ")

(def parser (insta/parser grammar))

(defn parse [input]
  (let
    [transform-map
     {:num (fn [& digits] (read-string (apply str digits)))
      :letter first
      :direction keyword
      }]
    (insta/transform transform-map (parser input))
    )
  )


(defn parse-input [path]
  (->>
    (slurp path)
    (parse)
    )
  )


(defn find-pos [s l]
  (loop [i 0]
    (if (= l (nth s i))
      i
      (recur (inc i)))))

(defn swap-pos [s x y]
  (let [a (nth s x)
        b (nth s y)]
    (assoc s x b y a)))

(defn swap-letter [s x y]
  (let [a (find-pos s x)
        b (find-pos s y)]
    (swap-pos s a b)))

(defn rotate [s dir i]
  (let [n (count s)
        i' (mod i n)
        ]
    (if (= :right dir)
      (let [n (count s)
            left (subvec s 0 (- n i'))
            right (subvec s (- n i'))
            ]
        (vec (concat right left))
        )
      (let [n (count s)
            left (subvec s 0 i')
            right (subvec s i')
            ]
        (vec (concat right left))
        )
      )
    )
  )

(defn rotate-pos [s l]
  (let [pos (find-pos s l)
        rotate-count (+ pos 1 (if (>= pos 4) 1 0))
        ]
    (rotate s :right rotate-count)
    )
  )

(defn reverse-str [s from to]
  (let [before (if (> from 0) (subvec s 0 from) [])
        after (if (< (inc to) (count s)) (subvec s (inc to)))
        reversed (reverse (subvec s from (inc to)))
        ]
    (vec (concat before reversed after))
    )
  )

(defn move [s from to]
  (let [from' (min from to)
        to' (max from to)
        dir (if (> to from) :left :right)
        before (subvec s 0 from')
        mid (subvec s from' (inc to'))
        after (subvec s (inc to'))
        ]
    (vec (concat before (rotate mid dir 1) after))
    ))


(defn exec-instruction [s i]
  (let [itype (first i)
        args (rest i)
        exec (case itype
               :swap-pos swap-pos
               :swap-letter swap-letter
               :rotate rotate
               :rotate-pos rotate-pos
               :reverse reverse-str
               :move-pos move
               (throw (Exception. (str "Unkown instruction " i)))
               )
        ]
    ;; (println s " -> exec " i)
    (apply exec s args))
  )


(defn solve1 []
  (let [instructions (parse-input "./resources/2016/day21.txt")]
    (println (apply str (reduce exec-instruction (vec "abcdefgh") instructions)))
    )
  )


(defn exec-reverse [s i]
  (let [itype (first i)
        args (rest i)
        ]
    (case itype
      :swap-pos (apply swap-pos s args)
      :swap-letter (apply swap-letter s args)
      :rotate (rotate s (if (= :left (first args)) :right :left) (second args))
      :rotate-pos (throw (Exception. "not done yet: rotate-pos"))
      :reverse (apply reverse-str s args)
      :move-pos (apply move s (reverse args))
      (throw (Exception. (str "Unkown instruction " i)))
      )
    )
  )

(defn solve2 [] (prn "nope"))
