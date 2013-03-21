(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn filterByTag [data tagName]
  (if (coll? data)
    (filter #(= (first %) tagName)(filter #(coll? %) data))))

(defn filterContainTag [data tagName]
  (if (coll? data)
    (if (= (first data) tagName)
      data
      [])))

(defn trav [data tagName filterFunc]
  (let [fData (filterFunc data tagName)]
    (if (empty fData)
      (if (coll? data)
        (loop [t data
               acc 0
               v []]
          (if (< acc (count t))
            (do 
              (recur t (inc acc) (concat v (trav (get t acc) tagName filterFunc))))
            (concat fData v)))))))

(defn googleResult[data] 
  (map #(:href (second %))(trav (trav data :h3 filterContainTag) :a filterByTag)))

(defn get-links []
  (let [data (parse "clojure_google.html")]
    (googleResult data))) 

(defn -main []
  (println (str "Found " (count (get-links)) " links!")))
