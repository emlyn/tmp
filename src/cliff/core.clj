(ns cliff.core
  (:require [net.cgrand.sjacket.parser :as p]
            [clojure.pprint :refer [pprint]]))

(defn munge-tree* [tree n]
  (if (string? tree)
    [{:node n :content tree} (inc n)]
    (let [[content nn] (reduce (fn [[content nn] node]
                                 (let [[c m] (munge-tree* node nn)]
                                   [(conj content c) m]))
                               [[] (inc n)]
                               (:content tree))]
      [(assoc tree :node n :content content) nn])))

(defn munge-tree [tree]
  (first (munge-tree* tree 0)))

(defn parse [fname]
  (-> fname slurp p/parser munge-tree))

(defn to-change-set [tree]
  (mapcat (fn [[c s]]
            (let [pcs {:parent (:node tree)
                       :child (:node c)
                       :successor (:node s)}]
              (cond
               (not   (:content c)) [pcs]
               (coll? (:content c)) (concat [pcs] (to-change-set c))
               :else                [pcs c])))
          (partition 2 1 (concat [{:node :left}] (:content tree) [{:node :right}]))))

(defn tmerge [tb t1 t2]
  )

(defn dumpraw [fname]
  (with-redefs [munge-tree identity]
    (pprint (parse fname))))

(defn dump [fname]
  (pprint (parse fname)))

(defn dumpcs [fname]
  (let [cs (to-change-set (parse fname))]
    (pprint cs)
    (println (count cs))))

(defn -main [fbase fedit1 fedit2]
  (let [tbase (parse fbase)
        tedit1 (parse fedit1)
        tedit2 (parse fedit2)]
    (pprint (tmerge tbase tedit1 tedit2))))
