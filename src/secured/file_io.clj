(ns secured.file-io
  (:require [clojure.java.io :as io :refer [reader file]])
  (:require [clojure.string :as s :refer [split-lines split lower-case replace]]))

(def password-data-path "./resources/password-data")

(def write-types {:txt {:ext ".txt" :path (str password-data-path "/txt") :data-func "data-newline" :append true}
		  :trie {:ext ".clj" :path (str password-data-path "/tries") :data-func "update-trie" :append false}})

(def special-chars "special-chars")

;;; PURE ;;;
(defn update-record 
    "Funtion adds :t 1 to result if not yet contained in trie. Otherwise, return result"
    [result record item]
    (if (not (contains? record :t))
      (update-in result (seq item) assoc :t 1)
      result))

(defn add-entry [result item]
  (let [record (get-in result item)]
  (if record
    (update-record result record item)
    (assoc-in result (get-in result item item) {:t 1}))))

(defn build-trie 
  "Builds a trie from a vector of strings - returns persistent array map 
  with chars as keys and {:t 1} in map where string terminates"
  [coll]
  (reduce add-entry {} coll))

(defn file-dir 
  "Returns canoncial path of a given path"
  [path]
  (.getCanonicalPath (io/file path)))

(defn illegal-starting-char? 
  "Any digit or number will return false, otherwise returns char (thus true)" 
  [letter]
  (or (re-find (re-pattern "[^\\w\\s]") (str letter)) false))

;; IO - individual files ;;
(defn str-to-filename [filename write-type]
  "Creates valid File type class from a filename string + write-type" 
  (io/file (file-dir (:path write-type)) (str filename (:ext write-type))))

(defn first-letter-filename 
  "Reads first character of a given string. If it is an illegal filename (i.e. *-!), it passes 'special-chars' to str-to-filename. Otherwise pass first character to str-to-filename e.g. 'apple' becomes 'a'"
   [line write-type]
   (let [first-char (str (first line))]
   (if (illegal-starting-char? first-char)
      (str-to-filename special-chars write-type) 
      (str-to-filename (lower-case first-char) write-type))))

(defn data-newline [line write-type]
  (str line "\r\n"))

(defn update-trie [line write-type]
  (let [read-file (str-to-filename (first line) write-type)]
  (if (.exists read-file) 
      (add-entry (read-string (slurp read-file)) line)
      (add-entry {} line))))

(defn set-data [line write-type]
  ((resolve (symbol (:data-func write-type))) line write-type))

(defn write-to-file 
   [write-file data write-type] 
       (spit write-file data :append (:append write-type)))

(defn write-first-letter-file 
   [read-file write-type] 
    (with-open [r (io/reader read-file)]
     (doseq [line (line-seq r)]
       (let [compacted-line (s/replace (str line) #" " "")]
       (let [write-file (first-letter-filename compacted-line write-type)]
       (write-to-file write-file (set-data compacted-line write-type) write-type))))))

;; IO - Directories ;;
(defn write-directory-to-files
  "Reads each filepath in a given directory into a seq and calls given function on it with a write-type if filepath is a file" 
  [read-dir write-type]
    (let [files (file-seq (io/file (file-dir read-dir)))]
    (doseq [f files] 
      (prn (str "Parsing: " f))
      (if-let [isFile (.isFile f)]
	(write-first-letter-file f write-type)))))

;; API
(defn compile-directory-to-tries 
"Takes the relative path to the directory you want to convert to tries. 
Requires existence of ./resources/password-data/ + txt + vectors + tries as prerequisite"
  [directory]
  (dosync 
    (prn "Compiling to txt files")
    (write-directory-to-files directory (:txt write-types))
    (prn "Building tries from txt files")
    (write-directory-to-files (get-in write-types [:txt :path]) (:trie write-types))
    (prn "Successfully completed")))

(defn find-repo [word]
  (let [first-char (first word)]
  (let [trie-type (:trie write-types)]
  (if (illegal-starting-char? first-char)
    (read-string (slurp (str-to-filename special-chars trie-type)))
    (read-string (slurp (str-to-filename first-char trie-type)))))))
