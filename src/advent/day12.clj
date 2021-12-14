(ns advent.day12
  (:require
   [clojure.string :as string]))

(defn visited-1 [visited node]
  )

(defn dfs [graph goal]
  (fn search [path visited]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> current
             (get graph)
             (remove visited)
             (mapcat #(search (conj path %)
                              (if (Character/isUpperCase (first %))
                                visited
                                (conj visited %)))))))))

(defn findpath [graph start goal]
  ((dfs graph goal) [start] #{start}))

(defn run [& _]
  (let [input (->> (string/split (slurp "inputs/day12") #"\n")
                   (map #(string/split % #"-"))
                   (reduce (fn [m [k v]] (let [m1 (if (contains? m k)
                                                    (update m k #(conj % v))
                                                    (assoc m k [v]))
                                               m2 (if (contains? m1 v)
                                                    (update m1 v #(conj % k))
                                                    (assoc m1 v [k]))]
                                           m2))
                           {}))]
    (->>
     (findpath input "start" "end")
     count
     (println "Part 1:"))))
