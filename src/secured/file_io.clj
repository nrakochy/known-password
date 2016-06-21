(ns secured.file-ioG
  (:require [clojure.java.io :as io :refer [reader file]])
  (:require [clojure.string :as s :refer [split-lines split]]))

(def data-path "./resources/data/txt")
(def trie-path "./resources/data/tries")
(def vec-path "./resources/data/vector")
(def clj-ext ".clj")

(defn file-dir 
  "Returns canonical path of a given path"
  [path] 
  (.getCanonicalPath (io/file path))) 

(defn illegal-starting-char? 
  "Any digit or number will return false, otherwise returns char (thus true)" 
  [letter]
  (or (re-find (re-pattern "[^\\w\\s]") (str letter)) false))

(defn first-letter-filename [line write-path ext]
   (let [special-chars "special-chars"]
   (let [first-char (first line)]
   (let [write-dir (file-dir write-path)]
   (if (illegal-starting-char? first-char)
      (io/file write-dir (str special-chars ext)) 
      (io/file write-dir (str (first line) ext)))))))

(defn write-first-letter-file 
  "Takes path to read file, desired write file extension, and optional write-path. 
   Takes each line of read file as sequence and creates file named 
   (first sequence) + given extension. If (first sequence) is an illegal filename,
   i.e. characters such as *, !, etc. the line will be written to \"special-chars\" + given extension.
   Defaults to writing to ./resources/data, but can be supplied different write path if desired."
  ([read-file write-ext] (write-first-letter-file read-file write-ext data-path))
  ([read-file write-ext write-path]
    (with-open [r (io/reader read-file)]
     (doseq [line (line-seq r)]
       (let [write-file (first-letter-filename line write-path write-ext)]
       (spit write-file (str line "\r\n") :append true))))))

(defn write-lines [read-file write-file]
  (let [data (s/split-lines (slurp read-file))] 
    (spit write-file data :append true)))

(defn full-file-path [write-path filename write-ext]
  (let [f-name (str (first (s/split filename #"[.]")) write-ext)]
  (io/file write-path f-name)))
 
(defn write-directory-to-files
  "Reads each file in a given directory into memory and calls given function on it" 
  [func read-dir write-path write-ext]
    (let [directory (io/file (file-dir read-dir))]
    (let [files (file-seq directory)]
    (doseq [f files] 
      (if-let [isFile (.isFile f)]
	(func f (full-file-path write-path (.getName f) write-ext)))))))
    
;;; TRIES  ;;;
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

(defn trie-to-file [read-file write-path]
  (let [arr (io/reader (slurp read-file))] 
  (prn arr)
  (let [data (build-trie arr)]
    (spit (full-file-path write-path clj-ext) data))))
 
(defn trie-directory [directory]
  (write-directory-to-file trie-to-file directory trie-path clj-ext))
