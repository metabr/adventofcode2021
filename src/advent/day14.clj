(ns advent.day14
  (:require
   [clojure.string :as string]))

(defn most-common-minus-least-common [polymer]
  (->> polymer
       (map #(vector (first (first %)) (second %)))
       (reduce (fn [m [k v]]
                 (if (contains? m k)
                   (update m k + v)
                   (conj m [k v])))
               {})
       vals
       (#(let [max (apply max %)
               min (apply min %)]
           (- max min)))))

(defn insert [rules init]
  (fn [polymer]
    (->> polymer
         (mapcat (fn [[pair count]]
                   (map #(hash-map % count) (rules pair))))
         (apply merge-with + init))))

(defn apply-steps [steps template rules init]
  (->> template
       (iterate (insert rules init))
       (drop steps)
       first))

(defn run [& _]
  (let [[polymer-template pair-insertion-rules]
        (string/split (slurp "inputs/day14") #"\n\n")
        last-element (last polymer-template)
        polymer-template (->> polymer-template
                              (partition 2 1)
                              frequencies)
        pair-insertion-rules (->> (string/split-lines pair-insertion-rules)
                                  (map #(string/split % #" -> "))
                                  (map (fn [[[l r] [m]]] {[l r] [[l m] [m r]]}))
                                  (into {}))
        polymerize (fn [steps] (apply-steps steps
                                            polymer-template
                                            pair-insertion-rules
                                            {[last-element] 1}))]

    (->> (polymerize 10)
         most-common-minus-least-common
         (println "After 10 steps: most common quantity - least common quantity ="))

    (->> (polymerize 40)
         most-common-minus-least-common
         (println "After 40 steps: most common quantity - least common quantity ="))))
