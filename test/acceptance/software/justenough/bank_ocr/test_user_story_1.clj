(ns software.justenough.bank-ocr.test-user-story-1
  "This namespace tests whether we can parse the OCR account numbers properly."
  (:require [clojure.string :as str]
            [clojure.test :as t :refer [deftest]]
            [software.justenough.bank-ocr.core :as ocr]
            [software.justenough.bank-ocr.test-util :as util]))

(deftest parse-account-numbers
  (let [test-values (repeatedly 500 util/gen-ocr-entry)]
    (doseq [{:keys [acc-num ocr]} test-values]
      (t/is (= acc-num
               (#'ocr/parse-entry (str/split-lines ocr)))))))
