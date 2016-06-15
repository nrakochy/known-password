(ns secured.fts
  (:require [clojure.string :as s :refer [split]]))

(defn build 
  [f coll result]
    (if (empty? coll)
      result
      (recur f (rest coll) (f result (first coll)))))

(defn add-key-if-missing [coll k]
  (if (not (contains? coll k))
    (assoc coll k [])
    coll))

(defn add-val [coll k v]
    (update-in coll [k] #(conj % v)))

(defn add-entry [coll pair]
  (let [k (last pair)]
  (let [v (first pair)]
  (if (not (contains? coll k))
    (assoc coll k [])
    (build get coll k)))))

;;; FTS ;;;
(defn index [coll]
  (let [mapped (map-indexed vector coll)]
  (build add-entry mapped {})))

(defn map-arbitrary-indexed [idx coll]
    (map #(conj [idx] (str %)) coll))


;;;; SUBSTRINGS ;;;;
(defn take-by-inc-index [idx word]
  (apply str (take (+ 1 idx) word)))

(defn substrings [word]
  (map-indexed (fn [idx & _] (take-by-inc-index idx word)) (seq word)))

;;;; TRIE ;;;;
(defn indexed-substrings [idx word]
  (let [possible-subs (substrings word)]
  (map-arbitrary-indexed idx possible-subs)))

(defn trie [coll]
  (let [concatted-substrings (apply concat (map-indexed #(indexed-substrings %1 %2) coll))]
  (build add-entry concatted-substrings {})))



