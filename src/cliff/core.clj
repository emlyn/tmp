(ns cliff.core
  (:require [net.cgrand.sjacket.parser :as p]
            [clojure.pprint :refer [pprint]]))

(defn maybevec [x]
  (if (= 1 (count x))
    (first x)
    (vec x)))

(defn striptree [tree]
  (cond
   (string? tree) tree
   (map?    tree) (recur (:content tree))
   (vector? tree) (maybevec (map striptree tree))
   :default       (throw (Exception. "Unknown parse tree element"))))

(defn -main [fname]
  (let [src (slurp fname)
        ptree (p/parser src)]
    (pprint (striptree ptree))))
