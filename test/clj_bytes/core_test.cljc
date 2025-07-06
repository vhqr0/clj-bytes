(ns clj-bytes.core-test
  (:require [clojure.test :refer [run-tests deftest testing is]]
            [clj-bytes.core :as b]
            [clj-bytes.struct :as st]
            #?@(:cljs [[clj-bytes.core :refer [bytes? byte-array aset-byte]]])))

(deftest base-test
  (testing "Core"
    (is (b/equals? (byte-array [1 2 3]) (byte-array [1 2 3])))
    (is (not (b/equals? (byte-array [1 2 3]) (byte-array [1 2 3 4]))))
    (is (bytes? (byte-array [1 2 3])))
    (is (= 3 (alength (byte-array [1 2 3]))))
    (is (= 2 (aget (byte-array [1 2 3]) 1)))
    (is (= [1 2 3] (vec (seq (byte-array [1 2 3])))))
    (is (b/equals?
         (byte-array [1 1 1])
         (byte-array 3 (byte 1))))
    (is (b/equals?
         (byte-array [1 2 3])
         (byte-array 3 [1 2 3 4])))
    (is (b/equals?
         (byte-array [1 2 3 0])
         (byte-array 4 [1 2 3])))
    (is (b/equals?
         (byte-array [4 2 3])
         (let [b (byte-array [1 2 3])]
           (aset-byte b 0 4)
           b))))
  (testing "Copy"
    (is (b/equals?
         (byte-array [3 4 3])
         (let [b1 (byte-array [1 2 3])
               b2 (byte-array [3 4])]
           (b/copy b2 0 b1 0 2)
           b1)))
    (is (b/equals?
         (byte-array [1 3 4])
         (let [b1 (byte-array [1 2 3])
               b2 (byte-array [3 4])]
           (b/copy b2 0 b1 1 2)
           b1))))
  (testing "Copy Of"
    (is (b/equals?
         (byte-array [1 2])
         (b/copy-of (byte-array [1 2 3]) 2)))
    (is (b/equals?
         (byte-array [1 2 3 0])
         (b/copy-of (byte-array [1 2 3]) 4)))
    (is (b/equals?
         (byte-array [2 3])
         (b/copy-of-range (byte-array [1 2 3]) 1 3)))
    (is (b/equals?
         (byte-array [2 3 0])
         (b/copy-of-range (byte-array [1 2 3]) 1 4))))
  (testing "Fill"
    (is (b/equals?
         (byte-array [4 4 4])
         (let [b (byte-array [1 2 3])]
           (b/fill b 4)
           b)))
    (is (b/equals?
         (byte-array [1 4 4])
         (let [b (byte-array [1 2 3])]
           (b/fill b 1 3 4)
           b))))
  (testing "Join"
    (is (b/equals?
         (byte-array [1 2 3 4])
         (b/join [(byte-array [1 2 3]) (byte-array [4])])))
    (is (b/equals?
         (byte-array [1 2 3])
         (b/join [(byte-array [1 2 3])]))))
  (testing "Index Of"
    (is (= 1 (b/index-of (byte-array [1 2 3]) (byte-array [2 3]))))
    (is (= 0 (b/index-of (byte-array [1 2 3]) (byte-array [1 2]))))
    (is (= -1 (b/index-of (byte-array [1 2 3]) (byte-array [1 3]))))))

(deftest int-test
  (is (b/equals?
       (byte-array [1 -128 3])
       (let [b (byte-array [1 2 3])]
         (b/put-ubyte b 1 128)
         b)))
  (is (b/equals?
       (byte-array [1 -128 -128])
       (let [b (byte-array [1 2 3])]
         (b/put-ushort b 1 0x8080)
         b)))
  (is (= 128 (b/get-ubyte (byte-array [1 -128 3]) 1)))
  (is (= 0x8080 (b/get-ushort (byte-array [1 -128 -128]) 1))))

(deftest str-test
  (is (b/equals?
       (byte-array [104 101 108 108 111])
       (->> "hello" b/str->bytes)))
  (is (= "hello" (-> (b/join [(byte-array [5]) (-> "hello" b/str->bytes)]) (b/get-str 1 5))))
  (is (= "hello" (-> "hello" b/str->bytes b/bytes->str)))
  (is (= "68656c6c6f" (-> "hello" b/str->bytes b/bytes->hex)))
  (is (= "hello" (-> "hello" b/str->bytes b/bytes->hex b/hex->bytes b/bytes->str)))
  (is (= "aGVsbG8=" (-> "hello" b/str->bytes b/bytes->mime-base64)))
  (is (= "hello" (-> "hello" b/str->bytes b/bytes->mime-base64 b/mime-base64->bytes b/bytes->str))))

(deftest struct-test
  (is (= "hello\r\n" (-> st/st-http-line (st/pack-one "hello") b/bytes->str)))
  (is (= ["hello" "world"] (-> (st/coll st/st-http-line) (st/unpack-one (b/str->bytes "hello\r\nworld\r\n"))))))
