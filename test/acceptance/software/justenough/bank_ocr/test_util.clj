(ns software.justenough.bank-ocr.test-util
  (:require [clojure.string :as str]))

(def valid-acc-nums
  '((2 8 1 0 0 0 8 1 3)
    (4 6 4 7 5 8 9 5 0)
    (8 4 3 8 1 0 6 5 3)
    (0 9 9 7 7 5 5 3 7)
    (6 2 3 7 9 2 8 6 6)
    (2 4 9 9 1 3 4 3 8)
    (2 4 2 1 4 7 7 5 3)
    (4 2 9 8 0 0 1 8 9)
    (6 5 4 5 1 7 7 4 0)))

(def invalid-acc-nums
  '((8 9 9 1 7 7 6 6 6)
    (5 2 7 1 2 0 8 2 8)
    (2 8 5 2 0 5 8 0 3)
    (2 1 6 1 4 8 1 7 7)
    (4 0 3 7 3 5 4 6 4)
    (6 0 4 6 8 0 1 8 0)
    (6 9 9 3 5 4 2 1 3)
    (2 4 1 7 6 7 1 9 9)
    (3 0 8 4 5 5 2 8 4)
    (8 7 7 0 8 6 1 2 2)
    (2 8 2 8 8 4 2 8 6)
    (8 2 6 3 2 3 0 8 8)
    (3 5 2 3 9 3 4 8 1)))

(def int->ocr
  {0 [\space \_ \space \| \space \| \| \_ \|]
   1 [\space \space \space \space \space \| \space \space \|]
   2 [\space \_ \space \space \_ \| \| \_ \space]
   3 [\space \_ \space \space \_ \| \space \_ \|]
   4 [\space \space \space \| \_ \| \space \space \|]
   5 [\space \_ \space \| \_ \space \space \_ \|]
   6 [\space \_ \space \| \_ \space \| \_ \|]
   7 [\space \_ \space \space \space \| \space \space \|]
   8 [\space \_ \space \| \_ \| \| \_ \|]
   9 [\space \_ \space \| \_ \| \space \_ \|]})

(defn gen-acc-seq
  ([] (gen-acc-seq 9))
  ([num]
   (repeatedly num #(rand-int 10))))

(defn- int-seq->ocr-entry
  [ocr-count acc-num]
  (->> acc-num
       (map int->ocr)
       (map #(partition 3 %))
       (apply interleave)
       (partition ocr-count)
       (map (partial apply concat))
       (map (partial apply str))
       (str/join "\n")))

(defn gen-ocr-entry
  "Return a map from `:acc-num` to the account number generated (as a seq of ints)
  and `:ocr` to the string OCR representation. Meant for testing."
  []
  (let [ocr-count 9 ; 9 digits in an account number
        acc-num   (gen-acc-seq ocr-count)]
    {:acc-num acc-num
     :ocr     (int-seq->ocr-entry ocr-count acc-num)}))
