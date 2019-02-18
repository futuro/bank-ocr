(ns software.justenough.bank-ocr.core
  "Parse OCR entries from files as defined in
  http://codingdojo.org/kata/BankOCR/."
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;
;; OCR Processing ;;
;;;;;;;;;;;;;;;;;;;;

(def ^:private ocr->int
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

(defn ^:private ocr-str->char-seqs
  "Given an entry of OCR number characters, return each numbers' characters in
  order.

  F.e.
  (\" _    \"
   \"| |  |\"
   \"|_|  |\")
  becomes
  `((\\space \\_ \\space \\| \\space \\| \\| \\_ \\|)
    (\\space \\space \\space \\space \\space \\| \\space \\space \\|))`

  N.B. Everything beyond the first three items in the passed in seq are dropped,
  to account for a possible empty fourth line in the entry spec."
  ([[row1 row2 row3 & _]]
   (ocr-str->char-seqs row1 row2 row3 []))
  ([row1 row2 row3 ocr-seqs]
   (if (or (empty? row1) (empty? row2) (empty? row3))
     ocr-seqs
     (recur (drop 3 row1) (drop 3 row2) (drop 3 row3) ; Advance past the number we're processing
            ;; Join the three rows of the number into a single seq of chars
            (conj ocr-seqs (concat (take 3 row1)
                                   (take 3 row2)
                                   (take 3 row3)))))))

(defn ^:private parse-entry
  "Given an OCR entry, return a seq of ints representing each digit in the entry.
  If an OCR digit can't be parsed, we return `:?` in its place."
  [entry]
  (->> entry
       (ocr-str->char-seqs)
       (map #(get ocr->int % :?))))

;;;;;;;;;;;;;;;;
;; Validation ;;
;;;;;;;;;;;;;;;;

;; The definition of this checksum, from the coding challenge, is described
;; essentially as the following:
;;
;; account number:  3  4  5  8  8  2  8  6  5
;; position names:  d9 d8 d7 d6 d5 d4 d3 d2 d1
;; checksum calculation:
;; ((d1+d2)*(d2+d3)*(d3+d4)...+d9*d9) mod 11 = 0
;;
;; The first thing to notice is that the first digit in the account number isn't
;; position `d1`, it's position `d9`.
;;
;; In english, you take the last number and the second to last number, sum them
;; together, then multiply the sum by the second to last number added to the
;; third to last number, and so on, until you've gotten to the first and second
;; numbers summed up and multiplied by the running product, finally multiplying
;; the first number with the product. Then return the product modulo 11.
;;
;; The second thing to notice, or remember, is that multiplication is
;; commutative, so you can multiple the sums together however you want.
;;
;; Following from this, we can realize that we don't need to reverse our seq of
;; ints, or jump through other hoops to calculate this checksum in the way
;; described, but instead can proceed through the digits d9->d1, summing each
;; successive pair and multiplying them against the running product.
;;
;; The product, in turn, can be set at the very beginning to the first
;; digit (which is position `d9`).
;;
;; XXX a funny side effect of the checkproduct algorithm as defined in
;; the codingdojo kata is that any two consecutive zeros in an
;; account number validates the whole account number. This feels
;; wrong, tbh, but I can't see any reason to believe it should be
;; different based on how User Story 2 is written
(defn ^:private checksum-entry
  "Calculate the checksum for a given account number."
  ([account-num]
   (mod (->> account-num
             (partition 2 1) ; This gives us the ((d1 d2) (d2 d3)...) groupings
             (map #(apply + %))
             (apply * (first account-num)))
        11)))

(defn ^:private valid-entry?
  "Does the given account number pass the checksum?"
  [account-num]
  (zero? (checksum-entry account-num)))

;;;;;;;;;;;;;;;;;;;
;; Orchestration ;;
;;;;;;;;;;;;;;;;;;;

(defn ^:private path->str-seqs
  "Given a path, return a seq of OCR entries."
  [path]
  (->> path
       (slurp)
       ;; XXX This will drop trailing newlines, so the last entry will be 3
       ;; lines instead of 4, thus the use of `partition-all` later
       (str/split-lines)
       (partition-all 4)))

(defn ^:private parse-file
  "Given a file path to properly formatted input, return the integers
  represented by the OCR characters."
  [path]
  (->> path
       path->str-seqs
       (map parse-entry)))

(defn ^:private parsed-entry->str
  "Given a parsed entry, return its string representation that's meant to be
  written to a file."
  [entry]
  (let [entry-str (str/join entry)]
    (cond
      (some #{:?} entry)         (str (str/replace entry-str ":" "") " ILL")
      (valid-entry? entry)       entry-str
      (not (valid-entry? entry)) (str entry-str " ERR"))))

;; TODO make this write the strings out to a file
(defn process-file
  "Given a path pointing to OCR entries, parse each entry and print out each
  account number followed by one of these strings:

  1. If the entry can be parsed and has a valid checksum, nothing
  2. If the entry can be parsed, but fails the checksum, \" ERR\"
  3. If the entry can't be parsed, \" ILL\""
  [in-path]
  (->> in-path
       parse-file
       (map parsed-entry->str)))
