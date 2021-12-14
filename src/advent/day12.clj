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

(defn dfs-2 [graph goal]
  (fn search [path visited twice]
    (let [current (peek path)]
      (if (= goal current)
        [path]
        (->> current
             (get graph)
             (remove visited)
             (mapcat #(search (conj path %)
                              (if (or (Character/isUpperCase (first %))
                                      (contains? twice %))
                                visited
                                (conj visited %))
                              (disj twice %))))))))

(defn findpath-2 [graph start goal]
  (let [small-caves (->> (mapcat val graph)
                         distinct
                         (filter #(not (= % "start")))
                         (filter #(not (= % "end")))
                         (filter #(Character/isLowerCase (first %))))]
    (mapcat #((dfs-2 graph goal) [start] #{start} #{%}) small-caves)))

(defn run [& _]
  (let [input
        (->> (string/split (slurp "inputs/day12") #"\n")
             (map #(string/split % #"-"))
             (reduce
              (fn [m [k v]]
                (let [m1 (if (contains? m k)
                           (update m k #(conj % v))
                           (assoc m k [v]))
                      m2 (if (contains? m1 v)
                           (update m1 v #(conj % k))
                           (assoc m1 v [k]))]
                  m2))
              {}))]
     (time
      (->>
       (findpath input "start" "end")
       count
       (println "Part 1:")))
     (time
      (->>
       (findpath-2 input "start" "end")
       distinct
       count
       (println "Part 2:")))))
