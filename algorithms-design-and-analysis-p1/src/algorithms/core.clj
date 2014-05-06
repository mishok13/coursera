(ns algorithms.core
  (:require
   [algorithms.week1 :as week1])
  (:gen-class))

(defn -main
  [week & args]
  (case week
    "1" (week1/solve (first args))))
