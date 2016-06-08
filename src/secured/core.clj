(ns secured.core
  (:require [secured.fts :refer [index trie]]))

(defn run [coll]
  (trie coll))
