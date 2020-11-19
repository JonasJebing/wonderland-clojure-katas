(ns alphabet-cipher.coder-test
  (:require [clojure.test :refer :all]
            [alphabet-cipher.coder :refer :all]))

(deftest test-zip
  (testing "can zip 2 sequences together"
    (is (= [[1 4] [2 5] [3 6]]
           (zip [1 2 3 9] [4 5 6])))))

(deftest test-char-range
  (testing "char-range a d"
    (is (= [\a \b \c \d]
           (char-range \a \d)))))

(deftest test-shift
  (testing "can shift an int sequence"
    (is (= [2 3 4 1]
           (shift [1 2 3 4]))))
  (testing "can shift an int sequence by n"
    (is (= [3 4 5 1 2]
           (shift 2 [1 2 3 4 5])))))

(deftest test-alphabet
  (testing "alphabet starts with abc"
    (is (= [\a \b \c]
           (take 3 alphabet)))))

(deftest test-shifted-alphabet
  (testing "shifted-alphabets starts with alphabet and alphabet shifted by 1"
    (is (= [alphabet (shift alphabet)]
           (take 2 shifted-alphabets))))
  (testing "shifted-alphabets ends with alphabet shifted by 25"
    (is (= (shift 25 alphabet)
           (last shifted-alphabets)))))

(deftest test-index-of
  (testing "index-of 3rd element is 2"
    (is (= 2
           (index-of [\a \b \c \d \e] \c)))))

(deftest test-encode-char
  (testing "m encoded with v is h"
    (is (= \h
           (encode-char \v \m)))))

(deftest test-encode
  (testing "can encode a message with a secret keyword"
    (is (= "hmkbxebpxpmyllyrxiiqtoltfgzzv"
           (encode "vigilance" "meetmeontuesdayeveningatseven")))
    (is (= "egsgqwtahuiljgs"
           (encode "scones" "meetmebythetree")))))

(deftest test-decode-char
  (testing "e decoded with s is m"
    (is (= \m
           (decode-char \s \e))))
  (testing "h decoded with v is m"
    (is (= \m
           (decode-char \v \h)))))

(deftest test-decipher-char
  (testing "m encoded as e is deciphered as s"
    (is (= \s
           (decipher-char \e \m)))))

(deftest test-decode
  (testing "can decode a message given an encoded message and a secret keyword"
    (is (= "meetmeontuesdayeveningatseven"
           (decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv")))
    (is (= "meetmebythetree"
           (decode "scones" "egsgqwtahuiljgs")))))

(deftest test-sub-slice
  (testing "can sub-slice abc"
    (is (= [[] [\a] [\a \b]]
           (sub-slice [\a \b \c])))))

(deftest test-find-keyword-repetition
  (testing "can find abc repetition in abcabca"
    (is (= [\a \b \c]
           (find-keyword-repetition [\a \b \c \a \b \c \a]))))
  (testing "can return the original sequence if no repetition is found"
    (is (= [\a \b \c]
           (find-keyword-repetition [\a \b \c]))))
  (testing "can resolve incomplete ab repetition in aba"
    (is (= [\a \b]
           (find-keyword-repetition [\a \b \a])))))

(deftest test-decipher
  (testing "can extract the secret keyword given an encrypted message and the original message"
    (is (= "vigilance"
           (decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog")))
    (is (= "scones"
           (decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs")))
    (is (= "abcabcx"
           (decipher "hfnlphoontutufa" "hellofromrussia")))))
