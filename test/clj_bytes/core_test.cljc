(ns clj-bytes.core-test
  (:require [clojure.test :refer [run-tests deftest testing is]]
            [clj-bytes.core :as sut]
            #?@(:cljs [[clj-bytes.core :refer [bytes? byte-array aset-byte]]])))

(deftest base-test
  (testing "Core"
    (is (sut/equals? (byte-array [1 2 3]) (byte-array [1 2 3])))
    (is (not (sut/equals? (byte-array [1 2 3]) (byte-array [1 2 3 4]))))
    (is (bytes? (byte-array [1 2 3])))
    (is (= 3 (alength (byte-array [1 2 3]))))
    (is (= 2 (aget (byte-array [1 2 3]) 1)))
    (is (= [1 2 3] (vec (seq (byte-array [1 2 3])))))
    (is (sut/equals?
         (byte-array [1 1 1])
         (byte-array 3 (byte 1))))
    (is (sut/equals?
         (byte-array [1 2 3])
         (byte-array 3 [1 2 3 4])))
    (is (sut/equals?
         (byte-array [1 2 3 0])
         (byte-array 4 [1 2 3])))
    (is (sut/equals?
         (byte-array [4 2 3])
         (let [b (byte-array [1 2 3])]
           (aset-byte b 0 4)
           b))))
  (testing "Copy"
    (is (sut/equals?
         (byte-array [3 4 3])
         (let [b1 (byte-array [1 2 3])
               b2 (byte-array [3 4])]
           (sut/copy b2 0 b1 0 2)
           b1)))
    (is (sut/equals?
         (byte-array [1 3 4])
         (let [b1 (byte-array [1 2 3])
               b2 (byte-array [3 4])]
           (sut/copy b2 0 b1 1 2)
           b1))))
  (testing "Copy Of"
    (is (sut/equals?
         (byte-array [1 2])
         (sut/copy-of (byte-array [1 2 3]) 2)))
    (is (sut/equals?
         (byte-array [1 2 3 0])
         (sut/copy-of (byte-array [1 2 3]) 4)))
    (is (sut/equals?
         (byte-array [2 3])
         (sut/copy-of-range (byte-array [1 2 3]) 1 3)))
    (is (sut/equals?
         (byte-array [2 3 0])
         (sut/copy-of-range (byte-array [1 2 3]) 1 4))))
  (testing "Fill"
    (is (sut/equals?
         (byte-array [4 4 4])
         (let [b (byte-array [1 2 3])]
           (sut/fill b 4)
           b)))
    (is (sut/equals?
         (byte-array [1 4 4])
         (let [b (byte-array [1 2 3])]
           (sut/fill b 1 3 4)
           b))))
  (testing "Join"
    (is (sut/equals?
         (byte-array [1 2 3 4])
         (sut/join [(byte-array [1 2 3]) (byte-array [4])])))
    (is (sut/equals?
         (byte-array [1 2 3])
         (sut/join [(byte-array [1 2 3])]))))
  (testing "Index Of"
    (is (= 1 (sut/index-of (byte-array [1 2 3]) (byte-array [2 3]))))
    (is (= 0 (sut/index-of (byte-array [1 2 3]) (byte-array [1 2]))))
    (is (= -1 (sut/index-of (byte-array [1 2 3]) (byte-array [1 3]))))))

(deftest int-test
  (is (sut/equals?
       (byte-array [1 -128 3])
       (let [b (byte-array [1 2 3])]
         (sut/put-ubyte b 1 128)
         b)))
  (is (sut/equals?
       (byte-array [1 -128 -128])
       (let [b (byte-array [1 2 3])]
         (sut/put-ushort b 1 0x8080)
         b)))
  (is (= 128 (sut/get-ubyte (byte-array [1 -128 3]) 1)))
  (is (= 0x8080 (sut/get-ushort (byte-array [1 -128 -128]) 1))))

(deftest str-test
  (is (sut/equals?
       (byte-array [104 101 108 108 111])
       (->> "hello" sut/str->bytes)))
  (is (= "hello" (-> "hello" sut/str->bytes sut/bytes->str)))
  (is (= "68656c6c6f" (-> "hello" sut/str->bytes sut/bytes->hex)))
  (is (= "hello" (-> "hello" sut/str->bytes sut/bytes->hex sut/hex->bytes sut/bytes->str)))
  (is (= "aGVsbG8=" (-> "hello" sut/str->bytes sut/bytes->mime-base64)))
  (is (= "hello" (-> "hello" sut/str->bytes sut/bytes->mime-base64 sut/mime-base64->bytes sut/bytes->str))))
