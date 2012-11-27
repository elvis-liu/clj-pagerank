(def damping-factor 0.85)
;(def out-links-map {:a [:c :d] :b [:a] :c [:d] :d [:b :e] :e [:b :d] :f [:c :d]})
(def out-links-map {:a [:c :d] :b [:a] :c [:d] :d [:b :e] :e [:b :d] :f [:c :d] :g [:b] :h [:g] :i [:g] :j [:g] :k [:g]})

(def in-links-map
  (reduce #(merge-with into %1 %2) (map #(zipmap %2 (repeat (count %2) [%1])) (keys out-links-map) (vals out-links-map))))

(def out-links-count-map (zipmap (keys out-links-map) (map count (vals out-links-map))))

(defn- calc-in-pr [prev-pr in-nodes]
  ; result = pr(b)/ol(b) + pr(c)/ol(c)
  (reduce + (map #(/ (get prev-pr %) (get out-links-count-map %)) in-nodes)))

(defn- calc-node [prev-pr in-nodes]
  ; pr(a) = (1 - d) + d * in-pr
  (+ (- 1 damping-factor) (* damping-factor (calc-in-pr prev-pr in-nodes))))

(defn- page-rank [prev-pr]
  (let [nodes (keys out-links-map)]
    (zipmap nodes (map #(calc-node prev-pr (get in-links-map %)) nodes))))

(defn- init-pr []
  (zipmap (keys out-links-map) (repeat (count out-links-map) 1)))

(defn- do-iter [prev-pr n max]
  (if (< n max)
    (let [new-pr (page-rank prev-pr)]
      (println n " " (sort-by second > new-pr))
      (recur new-pr (inc n) max))))

(do-iter (init-pr) 0 50)
