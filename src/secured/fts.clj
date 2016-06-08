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


;;; FTS ;;;
(defn index [coll]
  (let [mapped (map-indexed vector coll)]
  (build append-record mapped {})))


;;;; TRIE ;;;;
(defn take-by-inc-index [idx word]
  (apply str (take (+ 1 idx) word)))

(defn substrings [word]
  (map-indexed (fn [idx & _] (take-by-inc-index idx word)) (seq word)))

(defn map-arbitrary-indexed [idx coll]
    (map #(conj [idx] (str %)) coll))

(defn indexed-substrings [idx word]
  (let [possible-subs (substrings word)]
  (map-arbitrary-indexed idx possible-subs)))

(defn trie [coll]
  (let [concatted-substrings (apply concat (map-indexed #(indexed-substrings %1 %2) coll))]
  (build append-record concatted-substrings {})))

