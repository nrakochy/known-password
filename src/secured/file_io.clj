(ns secured.file-io
  (:require [clojure.java.io :as io :refer [reader file]])
  (:require [clojure.string :as s :refer [split-lines split]]))

(def password-data-path "./resources/password-data")
(def txt-path (str password-data-path "/txt"))
(def trie-path (str password-data-path "/tries"))
(def vec-path (str password-data-path "/vectors"))
(def clj-ext ".clj")
(def txt-ext ".txt")
(def special-chars "special-chars")

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

;; UTIL ;;
(defn file-dir 
  "Returns canonical path of a given path"
  [path] 
  (.getCanonicalPath (io/file path))) 

(defn illegal-starting-char? 
  "Any digit or number will return false, otherwise returns char (thus true)" 
  [letter]
  (or (re-find (re-pattern "[^\\w\\s]") (str letter)) false))

(defn first-letter-filename 
   [line write-path ext]
   (let [first-char (first line)]
   (let [write-dir (file-dir write-path)]
   (if (illegal-starting-char? first-char)
      (io/file write-dir (str special-chars ext)) 
      (io/file write-dir (str (first line) ext))))))

(defn full-file-path [write-path filename write-ext]
  (let [f-name (str (first (s/split filename #"[.]")) write-ext)]
  (io/file write-path f-name)))

;; Individual Files ;;
(defn write-first-letter-file 
  "Takes path to read file, desired write file extension, and optional write-path. 
   Takes each line of read file as sequence and creates file named 
   (first sequence) + given extension. If (first sequence) is an illegal filename,
   i.e. characters such as *, !, etc. the line will be written to \"special-chars\" + given extension.
   Defaults to writing to ./resources/data, but can be supplied different write path if desired."
  ([read-file write-ext] (write-first-letter-file read-file write-ext txt-path))
  ([read-file write-ext write-path]
    (with-open [r (io/reader read-file)]
     (doseq [line (line-seq r)]
       (let [write-file (first-letter-filename line write-path write-ext)]
       (spit write-file (str line "\r\n") :append true))))))

(defn trie-to-file [read-file write-file]
  (let [arr (read-string (slurp read-file))] 
  (let [data (build-trie arr)]
    (spit write-file data :append true))))

(defn vectorize-file [read-file write-path]
  (let [data (s/split-lines (slurp read-file))] 
    (spit write-path data :append true)))

(defn open-repo-file [string]
  (read-string (slurp (first-letter-filename string trie-path clj-ext))))

;; Directories ;;
(defn write-directory-to-files
  "Reads each filepath in a given directory into a seq and calls given function on it if it is a file" 
  [func read-dir write-path write-ext]
    (let [directory (io/file (file-dir read-dir))]
    (let [files (file-seq directory)]
    (doseq [f files] 
      (if-let [isFile (.isFile f)]
	(func f (full-file-path write-path (.getName f) write-ext)))))))

(defn split-file-to-txt-dir [file]
  (write-first-letter-file file txt-ext))

(defn vectorize-directory [directory]
  (write-directory-to-files vectorize-file directory vec-path clj-ext))

(defn build-tries-directory [directory]
  (write-directory-to-files trie-to-file directory trie-path clj-ext))

(defn compile-file-to-tries [filepath]
  (dosync 
    (prn "Compiling")
    (let [full-path (file-dir filepath)]
    (split-file-to-txt-dir full-path)
    (vectorize-directory (file-dir txt-path))
    (build-tries-directory (file-dir vec-path))
    (prn "Successfully completed"))))
