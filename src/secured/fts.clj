(ns secured.fts
  (:require [clojure.string :as s :refer [lower-case split]]))

(defn format-string [record]
 (-> record
  (str)
  (s/lower-case)))
  
(defn add-key-if-missing [coll k]
  (if (not (contains? coll k))
    (assoc coll k [])
    coll))

(defn add-val [coll k v]
  (update-in coll [k] #(conj % v)))

(defn append-record [coll indexed-pair] 
  (let [k (last indexed-pair)]
  (let [v (first indexed-pair)]
  (-> coll
    (add-key-if-missing k)
    (add-val k v)))))
  
(defn build 
  [f coll result]
    (if (empty? coll)
      result
      (recur f (rest coll) (f result (first coll)))))

(defn index [coll]
  (let [mapped (map-indexed vector coll)]
  (build append-record mapped {})))


;;;; TRIE ;;;;
(defn take-by-inc-index [idx word]
  (apply str (take (+ 1 idx) word)))

(defn substrings [word]
  (map-indexed (fn [idx & _] (take-by-inc-index idx word)) (seq word)))

(defn map-arbitrary-indexed [idx coll]
    (map #(conj [idx] %) coll))

(defn word-dict [idx word]
  {:index idx :substrings (substrings word) :entry word})

(defn minimum-prefixes [coll minimum])

(defn trie [coll]
  (let [substrings (map-indexed vector coll)]
  (build append-record substrings {})))

