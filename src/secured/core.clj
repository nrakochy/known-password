(ns secured.core
  (:require [secured.fts :refer [index trie build-tree]]))

(defn run [coll]
  (trie coll))
