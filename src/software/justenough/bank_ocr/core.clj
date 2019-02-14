(ns software.justenough.bank-ocr.core
  (:require [clojure.string :as str]))

(def numbers (str/split-lines (slurp "numbers.txt")))

(def ocr->int
  "This contains a map of the characters that make up every number to the
  integer they represent."
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

;; TODO think of a better name.
(defn ocr-str->char-seqs
  "Given an entry of OCR number characters, return each numbers' characters in
  order.

  F.e.
   _
  | |
  |_| becomes `(\\space \\_ \\space \\| \\space \\| \\| \\_ \\|)`"
  [entry]
  (loop [;; Convert the entry from one string per row into one seq per 3
         ;; characters, ordered by number
         numbers  (->> entry
                       (remove empty?)
                       (map #(partition 3 %))
                       (apply interleave))
         num-text (take 3 numbers)
         acc      []]
    (if (not-empty numbers)
      (recur (drop 3 numbers)
             (take 3 (drop 3 numbers))
             (conj acc (apply concat num-text)))
      acc)))

(defn entry->int-str
  [entry]
  (->> entry
       (ocr-str->char-seqs)
       (map ocr->int)
       (str/join)))

(defn process-entries
  [entries]
  (reduce (fn [num-strs entry]
            (conj num-strs (entry->int-str entry)))
          []
          entries))

(defn process-file
  [path]
  (->> path
       (slurp)
       (str/split-lines)
       (partition-all 4)
       (process-entries)))
