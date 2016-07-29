(ns secured.trie-test
  (:require [clojure.test :refer :all]
            [secured.trie :refer :all]))

(deftest finds-branch-with-matching-prefix-fn-find-branch-test
  (testing "Returns hash with matching prefix and the children branch of said prefix"
    (let [example "test"]
    (let [trie {\t {\e {\s {\t {:t 1 \i {\n {\g {:t 1}}}}}}}}]
    (let [result {:prefix "test" :trie {:t 1 \i {\n {\g {:t 1}}}}}]
    (is (= result (find-branch trie example))))))))

(deftest finds-branch-with-matching-prefix-fn-find-branch-test
  (testing "Returns hash with matching prefix and the children branch of said prefix"
    (let [example "test"]
    (let [trie {\t {\e {\s {\t {:t 1 \i {\n {\g {:t 1}}}}}}}}]
    (let [result {:prefix "test" :trie {:t 1 \i {\n {\g {:t 1}}}}}]
    (is (= result (find-branch trie example))))))))

(deftest finds-branch-with-inexact-matching-prefix-fn-drop-last-str-test
  (testing "Returns hash with nearest matching prefix and the children branch of said prefix"
    (let [example "tester"]
    (let [trie {\t {\e {\s {\t {:t 1 \i {\n {\g {:t 1}}}}}}}}]
    (let [result {:prefix "test" :trie {:t 1 \i {\n {\g {:t 1}}}}}]
    (is (= result (find-branch trie example))))))))

(deftest returns-string-from-vector-characters-ending-in-keyword-fn-results-str
  (testing "Returns prefix + string from vector of characters + one key"
    (let [example [\i \n \g :t]]
    (let [prefix "test"]
    (let [result "testing"] 
    (is (= result (results-str example prefix))))))))

(deftest returns-vector-strings-collapsing-vectors-fn-results-str
  (testing "Returns vector of string results" 
    (let [example [ [\e \r :t][\i \n \g :t] ]]
    (let [prefix "test"]
    (let [result ["tester" "testing"] ]
    (is (= result (results example prefix))))))))

(deftest returns-all-nested-children-keys-flattened-vector-fn-branch-keys-test
  (testing "Returns flatten keys in vector" 
    (let [trie {:t 1 \i {\n {\g {:t 1}}}}]
    (let [result [[:t] [\i \n \g :t]]] 
    (is (= result (branch-keys trie )))))))

(deftest returns-all-strings-which-share-branch-with-given-word-fn-search
  (testing "Returns strings which share branch with given word" 
    (let [word "test"] 
    (let [trie {\t {\e {\a {:t 1 \m {:t 1}} \s {\t {:t 1 \i {\n {\g {:t 1}}} \e {\r {:t 1}}}}}}}]
    (let [result ["test" "testing" "tester"]]
    (is (= result (search trie word))))))))
