(ns secured.trie) 

;;; BUILD ;;;
(defn add-entry [result item]
  (let [record (get-in result item)]
  (if record
    (update-in result (seq item) assoc :t 1)
    (assoc-in result (get-in result item item) {:t 1}))))

(defn build-trie 
  "Builds a trie from a vector of strings - returns persistent array map 
  with chars as keys and {:t 1} in map where string terminates"
  [coll]
  (reduce add-entry {} coll))

;;; SEARCH ;;;
(defn drop-last-str [word]
  (apply str (drop-last word)))

(defn find-branch [trie word]
  (let [branch (get-in trie word)]
  (if branch
    {:prefix word :trie branch} 
    (recur trie (drop-last-str word)))))

(defn results-str [arr prefix]
  (apply str prefix (drop-last arr)))

(defn results [coll prefix]
  (map #(results-str % prefix) coll))

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
