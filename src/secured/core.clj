(ns secured.core
  (:require [clojure.string :as s :refer [lower-case]]))

(defn format-string [record]
 (-> record
  (str)
  (s/lower-case)))
  
(defn add-key [coll pair]
  (let [k (last pair)]
  (if (not (contains? coll k))
    (assoc coll k [])
    coll)))

(defn add-val [coll pair]
  (prn (str coll))
  (let [idx (first pair)]
  (let [item (last pair)]
  (update-in coll [item] #(conj % idx)))))

(defn make-it [coll pair] 
  (-> coll
    (add-key pair)
    (add-val pair)))
  
(defn build-index 
  "Takes a vector and creates an inverted index of values as key and vector value with conj index"
  ([coll]  
    (let [mapped (map-indexed vector coll)]
    (build-index mapped {})))
  ([coll result]
    (if (empty? coll)
      result
      (recur (rest coll) (make-it result (first coll))))))

