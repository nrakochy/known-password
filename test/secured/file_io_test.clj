(ns secured.file-io-test
  (:require [clojure.test :refer :all]
            [secured.file-io :refer :all]))

(deftest returns-map-with-key-plus-value-appended-to-vec-given-fn-add-entry-test
  (testing "Inserts characters as keys of a given word and {:t 1} when given word terminates" 
    (let [example "test"]  
    (let [result {\t {\e {\s {\t {:t 1}}}}}]
    (is (= result (add-entry {} example)))))))

(deftest traverses-trie-before-adding-new-characters-fn-add-entry-test
  (testing "Traverses trie before adding new characters"
    (let [example "tester"]  
    (let [starting {\t {\e {\s {\t {:t 1}}}}}]
    (let [result {\t {\e {\s {\t {:t 1 \e {\r {:t 1}}}}}}}]
    (is (= result (add-entry starting example))))))))

(deftest adds-terminating-key-value-if-full-word-already-exists-in-trie-fn-add-entry-test
  (testing "Updates existing key-value map with {:t 1} if string already exists in trie"
    (let [example "te"]  
    (let [starting {\t {\e {\s {\t {:t 1}}}}}]
    (let [result {\t {\e {:t 1 \s {\t {:t 1}}}}}]
    (is (= result (add-entry starting example))))))))

(deftest builds-trie-from-vector-of-strings-fn-build-trie-test
  (testing "Builds trie"
    (let [example ["test" "testing" "testin"]]  
    (let [result {\t {\e {\s {\t {:t 1 \i {\n {\g {:t 1} :t 1}}}}}}}]
    (is (= result (build-trie {} example)))))))
