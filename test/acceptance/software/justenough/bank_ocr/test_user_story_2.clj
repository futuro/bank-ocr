(ns software.justenough.bank-ocr.test-user-story-2
  "The second user story was all about checksumming account numbers. While it
  didn't, strictly speaking, have acceptance criteria, I can think of some that
  may be worth exercising."
  (:require [clojure.test :as t :refer [deftest]]
            [software.justenough.bank-ocr.core :as ocr]
            [software.justenough.bank-ocr.test-util :as util]))

(deftest valid-account-numbers
  (doseq [acc-num util/valid-acc-nums]
    (t/is (ocr/valid-entry? acc-num))))

(deftest invalid-account-numbers
  (doseq [acc-num util/invalid-acc-nums]
    (t/is (not (ocr/valid-entry? acc-num)))))
