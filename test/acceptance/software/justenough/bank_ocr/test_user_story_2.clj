(ns software.justenough.bank-ocr.test-user-story-2
  "The second user story was all about checksumming account numbers. While it
  didn't, strictly speaking, have acceptance criteria, I can think of some that
  may be worth exercising."
  (:require [clojure.test :as t :refer [deftest]]
            [software.justenough.bank-ocr.core :as ocr]))

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

(deftest valid-account-numbers
  (doseq [acc-num valid-acc-nums]
    (t/is (ocr/valid-entry? acc-num))))

(deftest invalid-account-numbers
  (doseq [acc-num invalid-acc-nums]
    (t/is (not (ocr/valid-entry? acc-num)))))
