(ns advent-of-code.2016.10
  (:require [instaparse.core :as insta])
  (:require [clojure.string :as str]))

(def grammar "
  <line> = instruction | init
  instruction = <'bot '> num <' gives low to '> dest <' and high to '> dest
  init = <'value '> num <' goes to bot '> num
  <dest> = bot | output
  bot = <'bot '> num
  output = <'output '> num
  num = #'[0-9]+'
  ")

(def line-parser (insta/parser grammar))

(defn parse-line [l]
  (let [transform-map
        {:num (fn [& digits] (apply read-string digits))}
        ]
    (insta/transform transform-map (line-parser l))
    )
  )

(defn parse-input []
  (->>
    (slurp "resources/2016/day10.txt")
    (str/split-lines)
    (map parse-line)
    (apply concat)
    ))

(defn move-chip [env dest chip]
  (if (contains? env dest)
    (assoc env dest (conj (get env dest) chip))
    (assoc env dest [chip])
    )
  )

(defn build-env-reducer [env input]
  (if (= :init (first input))
    (let [[_ value bot] input]
      (move-chip env bot value))
    env
    )
  )

(defn build-env [lines]
  {:output {} :bot (reduce build-env-reducer {} lines)})

(defn build-instructions [lines]
  (reduce (fn [env [_ source t1 t2]] (assoc env source (list t1 t2))) {} (filter #(= :instruction (first %)) lines))
  )

(defn move-to [env source chip [target-type dest]]
  (let [old-val (get-in env [target-type dest] [])
        new-val (conj old-val chip)
        old-source (get-in env [:bot source])
        new-source (vec (filter #(not= chip %) old-source))]
    ;; (prn "env: " env)
    ;; (prn "moving from " source " chip: " chip " to " target-type ":" dest)
    ;; (prn "old val is: " old-val " new val is " new-val)
    ;; (prn "new source is: " new-source)
    (-> env
      (assoc-in [target-type dest] new-val)
      (assoc-in [:bot source] new-source)
      )
    ;; (assoc-in env [target-type dest] new-val)
    )
  )

;; (defn move-chips [env [c1 c2] [source [t1 t2]]]
(defn move-chips [env source [c1 c2] [t1 t2]]
  (let [low (min c1 c2)
        high (max c1 c2)]
    (prn (str/join "" ["moving " c1 " and " c2 " from " source]))
    (-> env
      (move-to source high t2)
      (move-to source low t1)
      )
    )
  )

(defn step [env instructions]
  (loop [current-env env remaining (seq (:bot env))]
    (if (empty? remaining)
      [false current-env]
      (let [[k v] (first remaining)]
        (if (= 2 (count v))
          [true (move-chips current-env k v (get instructions k))]
          ;; (recur (move-chips current-env k v (get instructions k)) (rest remaining))
          (recur current-env (rest remaining))
          )
        )
      )
    )
  )

(defn find-bot [bots val1 val2]
  (loop [bs (seq bots)]
    (if (empty? bs)
      nil
      (let [[bot-idx chips] (first bs)]
        ;; (prn (min val1 val2) (min chips) (max val1 val2) (max chips))
        (if (or (= chips [val1 val2]) (= chips [val2 val1]))
              ;; (= 2 (count chips))
              ;; (= (min chips) (min val1 val2))
              ;; (= (max chips) (max val1 val2))
              ;; )
          bot-idx
          (recur (rest bs))
          )
        )
      )
    )
  )


(defn iterate-and-find [env instructions val1 val2]
  (loop [current-env env]
    (let [[moved? new-env] (step current-env instructions)
          bot-idx (find-bot (:bot new-env) val1 val2)]
      ;; (prn "env:" env)
      ;; (prn "bot idx: " bot-idx)
      (if moved?
        (if (not= nil bot-idx)
          [new-env bot-idx]
          (recur new-env)
          )
        (do
          (prn "no more move")
          [new-env nil]
          )
        )
      )
    )
  )

(defn solve1 []
  (let [parsed (parse-input)
        env (build-env parsed)
        instructions (build-instructions parsed)]
    (prn (second (iterate-and-find env instructions 61 17)))
    )
  )


(defn solve2 []
  (let [parsed (parse-input)
        env (build-env parsed)
        instructions (build-instructions parsed)
        [final-env _] (iterate-and-find env instructions 0 0)
        out (:output final-env)]
    (prn (reduce * 1 (concat (out 0) (out 1) (out 2))))
    )
  )
