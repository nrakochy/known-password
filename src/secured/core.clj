(ns secured.core
  (:require [secured.fts :refer [index trie]])
  (:require [secured.file-io :refer [append-str]]))

(defn run [coll]
  (trie coll))
