(ns secured.file-io
  (:require [clojure.java.io :as io :refer [reader file]])
  (:require [clojure.string :as s :refer [split-lines split lower-case]]))

(def password-data-path "./resources/password-data")

(def write-types {:txt {:ext ".txt" :path (str password-data-path "/txt")}
		  :vec {:ext ".clj" :path (str password-data-path "/vectors")}
		  :trie {:ext ".clj" :path (str password-data-path "/tries")}})

(def special-chars "special-chars")

;;; TRIES  ;;;
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

;; UTIL ;;
(defn illegal-starting-char? 
  "Any digit or number will return false, otherwise returns char (thus true)" 
  [letter]
  (or (re-find (re-pattern "[^\\w\\s]") (str letter)) false))

;; Individual Files ;;
(defn str-to-filename [filename write-type]
  "Creates valid File type class from a filename string + write-type" 
  (io/file (file-dir (:path write-type)) (str filename (:ext write-type))))

(defn split-filename-ext [read-file write-type]
  "Splits a filename on dot character and passes it to str-to-filename + write-type"
  (let [fname (first (s/split (.getName read-file) #"[.]"))]
  (str-to-filename fname write-type)))

(defn first-letter-filename 
  "Reads first character of a given string. If it is an illegal filename (i.e. *-!), it passes 'special-chars' to str-to-filename. 
  Otherwise pass first character to str-to-filename e.g. 'apple' becomes 'a'"
   [line write-type]
   (let [first-char (first line)]
   (if (illegal-starting-char? first-char)
      (str-to-filename special-chars write-type) 
      (str-to-filename (lower-case (str (first line))) write-type)))) 

(defn write-first-letter-file 
  "Takes path to read file, desired write file extension, and optional write-path. 
   Takes each line of read file as sequence and creates file named 
   (first sequence) + given extension. If (first sequence) is an illegal filename,
   i.e. characters such as *, !, etc. the line will be written to \"special-chars\" + given extension.
   Defaults to writing to ./resources/data, but can be supplied different write path if desired."
  [read-file write-type] 
    (with-open [r (io/reader read-file)]
     (doseq [line (line-seq r)]
       (let [write-file (first-letter-filename line write-type)]
       (spit write-file (str line "\r\n") :append true)))))

(defn file-to-txt-dir [file write-type]
  (write-first-letter-file file write-type))
  
(defn trie-to-file [read-file write-type]
  (let [arr (read-string (slurp read-file))] 
  (let [data (build-trie arr)]
    (spit (split-filename-ext read-file write-type) data :append true))))

(defn vectorize-file [read-file write-type]
  (prn (str read-file))
  (let [data (s/split-lines (slurp read-file))] 
    (spit (split-filename-ext read-file write-type) data :append true)))

;; Directories ;;
(defn write-directory-to-files
  "Reads each filepath in a given directory into a seq and calls given function on it with a write-type if filepath is a file" 
  [func read-dir write-type]
    (let [files (file-seq (io/file (file-dir read-dir)))]
    (doseq [f files] 
      (if-let [isFile (.isFile f)]
	(func f write-type)))))

(defn unpack-password-directory [directory]
  (write-directory-to-files file-to-txt-dir directory (:txt write-types)))

(defn vectorize-directory [directory]
  (write-directory-to-files vectorize-file directory (:vec write-types)))

(defn build-tries-directory [directory]
  (write-directory-to-files trie-to-file directory (:trie write-types)))

(defn compile-file-to-tries 
"Takes the relative path to the directory you want to convert to tries. 
Requires existence of ./resources/password-data/ + txt + vectors + tries as prerequisite"
  [directory]
  (dosync 
    (prn "Compiling to txt files")
    (unpack-password-directory directory)
    (prn "Compiling to vectors")
    (vectorize-directory (get-in write-types [:txt :path]))
    (prn "Building tries from vectors")
    (build-tries-directory (get-in write-types [:vec :path]))
    (prn "Successfully completed")))
