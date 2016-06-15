(ns secured.trie) 

;;; BUILD ;;;
(defn add-entry [result item]
  (assoc-in result (get-in result item item) {:t 1}))

(defn build-trie 
  "Builds a trie from a vector of strings - returns persistent array map 
  with chars as keys and {:t 1} in map where string terminates"
  [coll]
  (reduce add-entry {} coll))

;;; SEARCH ;;;
(defn iterate-from-str-end [word]
  (apply str (take (- (count word) 1) word)))

(defn find-branch [trie word]
  (let [branch (get-in trie word)]
  (if branch
    {:prefix word :trie branch} 
    (recur trie (iterate-from-str-end word)))))

(defn combine-str [arr prefix]
  (apply str prefix (drop-last arr)))

(defn results [coll prefix]
  (map #(combine-str % prefix) coll))

(defn branch-keys
  ([trie]
    (branch-keys {} [] trie))
  ([store result trie]
    (if (map? trie)
     (reduce into (map (fn [[k v]] (branch-keys store (conj result k) v)) (seq trie)))
     (conj [] result))))

(defn search [trie query] 
  (let [branch (find-branch trie query)]
  (let [matching-keys (branch-keys (:trie branch))]
  (results matching-keys (:prefix branch)))))
