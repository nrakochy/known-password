(ns secured.fts-test
  (:require [clojure.test :refer :all]
            [secured.fts :refer :all]))

(deftest correctly-adds-key-fn-add-key-if-missing-test
  (testing "Associates key + empty vector to given hash if key does not exist"
    (let [example-coll {"example1" []}]  
    (let [example-k "example2"]  
    (let [result {"example1" [] "example2" []}]  
    (is (= result (add-key-if-missing example-coll example-k))))))))

(deftest returns-collection-unchanged-if-key-already-exists-fn-add-key-if-missing-test
  (testing "Associates key + empty vector to given hash if key does not exist"
    (let [example-coll {"example1" []}]  
    (let [example-k "example1"]  
    (let [result example-coll]  
    (is (= result (add-key-if-missing example-coll example-k))))))))

(deftest conjoins-value-to-vector-value-of-given-key-fn-add-val-test
  (testing "Appends given value to value vector of looked up key"
    (let [example-coll {"example1" [2]}]  
    (let [example-k "example1"]  
    (let [example-v 1]  
    (let [result {"example1" [2 1]}]  
    (is (= result (add-val example-coll example-k example-v)))))))))

(deftest returns-map-with-key-plus-value-appended-to-vec-given-fn-add-entry-test
  (testing "Appends given value to vector of preexisting key"
    (let [example-coll {"example1" [2]}]  
    (let [example-pair [1 "example1"]]  
    (let [result {"example1" [2 1]}]  
    (is (= result (add-entry example-coll example-pair))))))))

(deftest adds-key-plus-value-appended-to-vec-fn-add-entry-test
  (testing "Inserts key + [value] if key does not exist" 
    (let [example-coll {"example2" [2]}]  
    (let [example-pair [1 "example1"]]  
    (let [result {"example2" [2] "example1" [1]}]  
    (is (= result (add-entry example-coll example-pair))))))))

(deftest returns-map-result-recursive-given-function-call-fn-build-test
  (testing "Builds hash map recursively executing given function"   
    (let [example-coll [[0 "example1"] [1 "example1"] [2 "example2"]]]  
    (let [example-fn add-entry]
    (let [empty-start {}]
    (let [result {"example2" [2] "example1" [0 1]}]  
    (is (= result (build example-fn example-coll empty-start)))))))))

(deftest returns-inverted-hash-string-keys-vector-indices-fn-index-test
  (testing "Returns inverted hash with vector of indices where string is located"
    (let [example-coll ["example1" "example1" "example2"]]  
    (let [result {"example2" [2] "example1" [0 1]}]  
    (is (= result (index example-coll)))))))

(deftest returns-inverted-hash-string-keys-vector-indices-fn-index-test
  (testing "Returns inverted hash with vector of indices where string is located"
    (let [example-coll ["example1" "example1" "example2"]]  
    (let [result {"example2" [2] "example1" [0 1]}]  
    (is (= result (index example-coll)))))))

(deftest returns-vector-substrings-incrementing-given-index-fn-take-by-inc-index
  (testing "Returns vector with a string whose length is the given number + 1" 
    (let [example "test"]
    (let [example-index 1]
    (let [result "te"] 
    (is (= result (take-by-inc-index example-index example))))))))

(deftest returns-collection-possible-substrings-starting-at-index0-fn-substrings-test
  (testing "Returns vector possible substrings" 
    (let [example "test"]
    (let [result ["t" "te" "tes" "test"]]
    (is (= result (substrings example)))))))

(deftest returns-2d-vector-item-given-index-fn-map-arbitrary-indexed-test
  (testing "2d vector of given index + item in map"
    (let [index 1]
    (let [example ["t" "te" "tes" "test"]]
    (let [result [[index "t"] [index "te"] [index "tes"] [index "test"]]]
    (is (= result (map-arbitrary-indexed index example))))))))

(deftest returns-sequence-indexed-substrings-entry-fn-map-arbitrary-indexed-test
  (testing "Returns prepended index and item in a vector"
    (let [index 1]
    (let [word "test"]
    (let [result [[1 "t"] [1 "te"] [1 "tes"] [1 "test"]]]
    (is (= result (indexed-substrings index word))))))))

(deftest returns-trie-of-substring-occurences-fn-trie-test
  (testing "Returns trie of substrings with vector of indices where located in the collection" 
    (let [example ["tee" "tea" "team"]]
    (let [result {"t" [0 1 2] "te" [0 1 2] "tee" [0] "tea" [1 2] "team" [2]}]
    (is (= result (trie example)))))))










