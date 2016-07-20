(ns secured.file-io
  (:require [clojure.java.io :as io :refer [reader file]])
  (:require [clojure.string :as s :refer [split-lines split lower-case replace]]))

(def password-data-path "./resources/password-data")

(def write-types {:txt {:ext ".txt" :path (str password-data-path "/txt") :data-func "data-newline" :append true :async false}
		  :trie {:ext ".clj" :path (str password-data-path "/tries") :data-func "update-trie" :append false :async true}})

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
  ([coll] reduce add-entry {} coll)
  ([coll result] reduce add-entry result coll))

(defn file-dir 
  "Returns canoncial path of a given path"
  [path]
  (.getCanonicalPath (io/file path)))

(defn illegal-starting-char? 
  "Any digit or number will return false, otherwise returns char (thus true)" 
  [letter]
  (or (re-find (re-pattern "[^\\w\\s]") (str letter)) false))

(defn str-to-filename [write-type filename]
  "Creates valid File type class from a filename string + write-type" 
  (io/file (file-dir (:path write-type)) (str filename (:ext write-type))))

(defn data-newline [line write-type]
  (str line "\r\n"))

(defn compact-line [line]
  (s/replace (str line) #" " ""))

(defn set-data [write-type data]
  (let [func (symbolize write-type)]
  ((resolve (symbol (:data-func write-type))) data write-type)))

;; IO ;;
(defn first-letter-filename 
  "Reads first character of a given string. If it is an illegal filename (i.e. *-!), it passes 'special-chars' to str-to-filename. Otherwise pass first character to str-to-filename e.g. 'apple' becomes 'a'"
   [write-type line]
   (let [first-char (str (first line))]
   (if (illegal-starting-char? first-char)
      (str-to-filename write-type special-chars) 
      (str-to-filename write-type (lower-case first-char)))))

(defn update-trie [coll write-type]
  (let [read-file (str-to-filename write-type (first (first coll)))]
  (if (.exists read-file) 
      (build-trie (read-string (slurp read-file)) coll)
      (build-trie coll))))

(defn write-to-file 
   [file data write-type] 
       (spit file data :append (:append write-type)))

(defn save-record [write-type line]
  (write-to-file (first-letter-filename write-type line) (set-data write-type line) write-type))

(defn save-batch [write-type coll]
  (let [first-record (first coll)]
  (write-to-file (first-letter-filename write-type first-record) (set-data write-type coll) write-type)))

(defn persist [write-type lines]
  (cond
  (:async write-type) (save-batch write-type lines) 
  :default (dorun (map #(save-record write-type %) lines))))

(defn first-letter-file 
   [read-file write-type] 
    (with-open [r (io/reader read-file)]
     (->> (line-seq r)
	 (map compact-line) 
	 (partition-all 10000)
	 (#(doseq [lines %]
	    (persist write-type lines))))))

;; IO - Directories ;;
(defn directory-to-files
  "Reads each filepath in a given directory into a seq and calls given function on it with a write-type if filepath is a file" 
  [read-dir write-type]
    (let [files (file-seq (io/file (file-dir read-dir)))]
    (doseq [f files] 
      (prn (str "Parsing: " f))
      (if-let [isFile (.isFile f)]
	(first-letter-file f write-type)))))

;; API
(defn compile-directory-to-tries 
"Takes the relative path to the directory you want to convert to tries. 
Requires existence of ./resources/password-data/ + txt + vectors + tries as prerequisite"
  [directory]
  (dosync 
    (prn "Compiling to txt files")
    (directory-to-files directory (:txt write-types))
    (prn "Building tries from txt files")
    (directory-to-files (get-in write-types [:txt :path]) (:trie write-types))
    (prn "Successfully completed")))

(defn find-repo [word]
  (let [first-char (first word)]
  (let [trie-type (:trie write-types)]
  (if (illegal-starting-char? first-char)
    (read-string (slurp (str-to-filename trie-type special-chars)))
    (read-string (slurp (str-to-filename trie-type first-char)))))))
