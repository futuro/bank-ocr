(ns software.justenough.bank-ocr.core
  (:require [clojure.string :as str]))

(def ocr->int
  "This contains a map of the sequence of characters that make up every OCR number
  to the integer they represent."
  {[\space \_ \space \| \space \| \| \_ \|]                 0
   [\space \space \space \space \space \| \space \space \|] 1
   [\space \_ \space \space \_ \| \| \_ \space]             2
   [\space \_ \space \space \_ \| \space \_ \|]             3
   [\space \space \space \| \_ \| \space \space \|]         4
   [\space \_ \space \| \_ \space \space \_ \|]             5
   [\space \_ \space \| \_ \space \| \_ \|]                 6
   [\space \_ \space \space \space \| \space \space \|]     7
   [\space \_ \space \| \_ \| \| \_ \|]                     8
   [\space \_ \space \| \_ \| \space \_ \|]                 9})

(defn ocr-str->char-seqs
  "Given an entry of OCR number characters, return each numbers' characters in
  order.

  F.e.
   _
  | |
  |_| becomes `((\\space \\_ \\space \\| \\space \\| \\| \\_ \\|))`"
  [entry]
  (loop [numbers (->> entry
                      ;; An entry may have an empty line, which we don't want
                      ;; to keep
                      (remove empty?)
                      ;; Each OCR number has 3 characters per row; split them
                      ;; into individual seqs
                      (map #(partition 3 %))
                      ;; Each OCR number has 3 rows; order each row's seq one
                      ;; after the other, top-to-bottom and left-to-right
                      (apply interleave))
         acc     []]
    (if (not-empty numbers)
      (recur (drop 3 numbers) ; Drop the OCR number we're processing from the collection
             (conj acc (apply concat (take 3 numbers))))
      acc)))

(defn entry->int-str
  "Given an entry of OCR characters, return the integer it represents as a
  string."
  [entry]
  (->> entry
       (ocr-str->char-seqs)
       (map ocr->int)
       (str/join)))

(defn process-file
  "Given a file path to properly formatted input, return the integers
  represented by the OCR characters."
  [path]
  (->> path
       (slurp)
       ;; XXX This will drop trailing newlines, so the last entry will be 3
       ;; lines instead of 4
       (str/split-lines)
       (partition-all 4) ; Each entry is 4 lines, except the last
       (map entry->int-str)))
