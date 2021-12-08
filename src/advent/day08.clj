(ns advent.day08
  (:require
   [clojure.string :as string]))

(defn process-signal [s]
  (->> (sort s)
       (apply str)))

(defn known-digit? [s]
  (case (count s)
    2 [1 s]
    3 [7 s]
    4 [4 s]
    7 [8 s]
    nil))

(defn contains? [digit signals]
  (let [r (map (fn [s] (some #(= s %) digit))
               signals)]
    (if (some nil? r)
      false
      true)))

(defn is-6-or-9? [s known]
  (if (== 6 (count s))
    (if (contains? s (known 1))
      (if (contains? s (known 4))
        [9 s] [0 s])
      [6 s])))

(defn analyze-rest [s known]
  (if (== 5 (count s))
    (if (contains? s (known 1))
      [3 s]
      ;; 2 or 5
      (if (contains? (known 9) s)
        [5 s] [2 s]))))

(defn analyze [{:keys [signal-patterns output-digits]}]
  (let [known-digits (->> (keep known-digit? signal-patterns)
                          (apply concat)
                          (apply hash-map))
        known-digits (apply conj known-digits
                            (keep #(is-6-or-9? % known-digits) signal-patterns))
        known-digits (apply conj known-digits
                            (keep #(analyze-rest % known-digits) signal-patterns))
        signal->digit (zipmap (vals known-digits) (keys known-digits))
        ]
    (->>
     (map #(get signal->digit %) output-digits)
     (apply str)
     Integer/parseInt)))

(defn run [& _]
  (let [input (->> (string/split (slurp "inputs/day08") #"\n")
                   (map #(string/split % #" \| "))
                   (map (fn [[signals digits]]
                          {:signal-patterns
                           (map process-signal (string/split signals #" "))
                           :output-digits
                           (map process-signal (string/split digits #" "))})))
        is-1-4-7-or8? (fn [digit]
                        (let [len (count digit)]
                          (or (== len 2) ; this is 1
                              (== len 3) ; this is 7
                              (== len 4) ; this is 4
                              (== len 7) ; this is 8
                              )))]
    (->> input
         (map :output-digits)
         (apply concat)
         (filter is-1-4-7-or8?)
         count
         (println "1, 4, 7 or 8 appear this many times:"))
    (->> input
         (map analyze)
         (reduce + 0)
         (println "Sum of output values:"))))
