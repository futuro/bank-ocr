(ns software.justenough.bank-ocr.test-util
  (:require [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;
;; Test Helpers ;;
;;;;;;;;;;;;;;;;;;

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

(defn gen-ocr-entry
  "Return a map from `:acc-num` to the account number generated (as a seq of ints)
  and `:ocr` to the string OCR representation. Meant for testing."
  []
  (let [ocr-count 9 ; 9 digits in an account number
        acc-num   (gen-acc-seq ocr-count)]
    {:acc-num acc-num
     :ocr     (->> acc-num
                   (map int->ocr)
                   (map #(partition 3 %))
                   (apply interleave)
                   (partition ocr-count)
                   (map (partial apply concat))
                   (map (partial apply str))
                   (str/join "\n"))}))
