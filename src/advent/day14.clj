(ns advent.day14
  (:require
   [clojure.string :as string]))

(defn polymerize [rules template]
  (let [template (into [] template)
        f (vector (first template))
        r (pop (into [] (rest template)))
        l (vector (peek template))]
    (->>
     (concat
      f
      (mapcat #(vector % %) r)
      l)
     (partition 2)
     (map #(apply str %))
     (map (fn [k] [(get rules k) (second k)]))
     (apply (partial concat f))
     (apply str))))

(defn most-common-minus-least-common [polymer]
  (->> polymer frequencies vals
       (#(let [max (apply max %)
               min (apply min %)]
           (- max min)))))

(defn count-elements
  "Return difference of most- and least-common elements."
  [polymer]
  (->> polymer
       (map (fn [[k v]] {(first k) v}))   ; Can't use `update-keys` because
       (apply merge-with +)               ; collisions; could use `reduce-kv`.
       vals
       (apply (juxt max min))
       (apply -)))

(defn run [& _]
  (let [input (string/split-lines (slurp "inputs/day14"))
        polymer-template (first input)
        pair-insertion-rules
        (->> (drop 2 input)
             (map #(string/split % #" -> "))
             (mapcat #(vector (first %) (first (second %))))
             (apply hash-map))
        polymerize (partial polymerize pair-insertion-rules)
        polymer (iterate polymerize polymer-template)]
    (->> (drop 10 polymer) first most-common-minus-least-common
         (println "After 10 steps: most common quantity - least common quantity ="))))
