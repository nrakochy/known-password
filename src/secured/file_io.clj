(ns secured.file-io
  (:require [clojure.java.io :as io :refer [reader file]])
  (:require [clojure.string :as s :refer [split-lines lower-case replace]]))

(def password-data-path "./resources/password-data")

(def write-types {:txt {:ext ".txt" :path (str password-data-path "/txt") :data-func "data-newline" :append true :async false}
		  :trie {:ext ".clj" :path (str password-data-path "/tries") :data-func "update-trie" :append false :async true}})

(def special-chars "special-chars")

;;; PURE ;;;
(defn add-entry [result item]
  (if (get-in result item) 
    (update-in result (apply str item) assoc :t 1)
    (assoc-in result (get-in result (apply str item) (apply str item)) {:t 1})))

(defn build-trie 
  "Builds a trie from a vector of strings - returns persistent array map 
  with chars as keys and {:t 1} in map where string terminates"
  [result coll] 
  (loop [result {} c (seq coll)]
    (if (or (empty? c) (nil? c))
        result
	(recur (add-entry result (first c)) (rest c)))))

(defn update-trie [coll write-type]
  (build-trie {} coll))

(defn file-dir 
  "Returns canoncial path of a given path"
  [path]
  (.getCanonicalPath (io/file path)))

(defn illegal-starting-char? 
  "Any digit or number will return false, otherwise returns char (thus true)" 
  [letter]
  (or (re-find (re-pattern "[^\\w\\s]") (str letter)) false))

(defn legal-starting-chars? 
  "Returns boolean based on result of checking result sequence of the fn illegal-starting-char? on a collection"
  [string]
  (every? false? (map illegal-starting-char? (seq string))))

(defn str-to-filename [write-type filename]
  "Creates valid File type class from a filename string + write-type" 
  (io/file (file-dir (:path write-type)) (str filename (:ext write-type))))

(defn data-newline [line write-type]
  (if (not (empty? line))
    (str line "\r\n")))

(defn compact-line [line]
  (s/replace (str line) #" " ""))

(defn set-data [write-type data]
  ((resolve (symbol (:data-func write-type))) data write-type))

;; IO ;;
(defn read-trie-file [file] 
  (read-string (slurp file))) 

(defn truncate [string]
  (lower-case (apply str (take 2 string))))

(defn set-filename 
  "Reads first character of a given string. If it is an illegal filename (i.e. *-!), it passes 'special-chars' to str-to-filename. Otherwise pass first character to str-to-filename e.g. 'apple' becomes 'a'"
   [write-type line]
   (if (legal-starting-chars? (truncate line))
      (str-to-filename write-type (truncate line))
      (str-to-filename write-type special-chars))) 

(defn write-to-file 
   [file data write-type] 
       (spit file data :append (:append write-type)))

(defn save-record [write-type line]
  (write-to-file (set-filename write-type line) (set-data write-type line) write-type))

(defn save-batch [write-type coll]
  (write-to-file (set-filename write-type (first coll)) (set-data write-type coll) write-type))

(defn persist [write-type lines]
  (cond
  (:async write-type) (dorun (save-batch write-type lines)) 
  :default (dorun (map #(save-record write-type %) lines))))

(defn process-file 
   [read-file write-type] 
    (with-open [r (io/reader read-file)]
     (->> (line-seq r)
	 (map compact-line) 
	 (partition-all 5000)
	 (#(doseq [lines %]
	    (persist write-type lines))))))

(defn process-in-mem 
   [read-file write-type] 
    (with-open [r (io/reader read-file)]
     (->> (line-seq r)
	  (persist write-type))))

;; IO - Directories ;;
(defn directory-to-files
  "Reads each filepath in a given directory into a seq and calls given function on it with a write-type if filepath is a file" 
  [read-dir write-type]
    (doseq [f (file-seq (io/file (file-dir read-dir)))] 
      (prn (str "Parsing: " f))
      (if (.isFile f)
	(process-file f write-type))))

;; IO - Directories ;;
(defn directory-to-files-in-mem
  [read-dir write-type]
    (doseq [f (file-seq (io/file (file-dir read-dir)))] 
      (prn (str "Parsing: " f))
      (if (.isFile f) 
	  (process-in-mem f write-type))))

;; API
(defn compile-directory-to-tries 
"Takes the relative path to the directory you want to convert to tries. 
Requires existence of ./resources/password-data/ + txt/ & /tries as prerequisite"
  [directory]
  (dosync 
    (prn "Compiling to txt files")
    (directory-to-files directory (:txt write-types))
    (prn "Building tries from txt files")
    (directory-to-files-in-mem (get-in write-types [:txt :path]) (:trie write-types))
    (prn "Successfully completed")))

(defn find-repo [word]
  (let [repo-abbrv (truncate word)]
  (let [trie-type (:trie write-types)]
  (if (legal-starting-chars? repo-abbrv)
    (read-trie-file (str-to-filename trie-type repo-abbrv))
    (read-trie-file (str-to-filename trie-type special-chars))))))
