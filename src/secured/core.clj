(ns secured.core
  (:require [secured.trie :refer [present-in-trie? search]])
  (:require [secured.file-io :refer [compile-directory-to-tries find-repo]]))

(defn compile-password-tries 
  "A CLI utility for compiling a directory of txt files of passwords (newline delimited) into a directory of clojure tries. Requires project_root/resources/password-data/ + txt/ vectors/ tries/ as prerequisite directories"
  [directory]
  (compile-directory-to-tries directory))

(defn check-password [word]
 (let [trie (find-repo word)]
 (let [related-passwords {:related-passwords (into [] (search trie word))}]
  (if (present-in-trie? trie word)
    (assoc related-passwords :known-password? true)
    (assoc related-passwords :known-password? false)))))

