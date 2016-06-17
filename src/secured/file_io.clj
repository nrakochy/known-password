(ns secured.file-io
  (:require [clojure.java.io :as io :refer [reader file]]))

(def data-path "./resources/data")

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
 
