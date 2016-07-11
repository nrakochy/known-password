(ns secured.core
  (:require [secured.trie :refer [present-in-trie? search]])
  (:require [secured.file-io :refer [compile-file-to-tries open-repo-file]]))

(defn compile-password-tries 
  "A CLI utility for compiling a text file of passwords (newline delimited) into a directory of clojure tries."
  [filepath]
  (compile-file-to-tries filepath))

(defn check-password [word]
 (let [trie (open-repo-file word)]
 (let [related-passwords {:related-passwords (into [] (search trie word))}]
  (if (present-in-trie? trie word)
    (assoc related-passwords :known-password? true)
    (assoc related-passwords :known-password? false)))))

