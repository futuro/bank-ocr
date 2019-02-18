(ns software.justenough.bank-ocr.test-user-story-3
  (:require [clojure.test :as t :refer [deftest]]
            [software.justenough.bank-ocr.core :as ocr]
            [clojure.string :as str]
            [software.justenough.bank-ocr.test-util :as util]))

(deftest valid-account-numbers
  (let [processed-entries (ocr/process-file "valid-entries.txt")]
    (t/is (= (map str/join util/valid-acc-nums)
             processed-entries))))

(deftest invalid-account-numbers
  (let [processed-entries (ocr/process-file "invalid-entries.txt")]
    (t/is (= (map #(str (str/join %) " ERR") util/invalid-acc-nums)
             processed-entries))))

(deftest illegible-account-numbers
  (let [processed-entries (ocr/process-file "illegible-entries.txt")]
    (t/is (every? #(str/ends-with? % " ILL") processed-entries))))
