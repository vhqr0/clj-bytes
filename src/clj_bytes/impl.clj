(ns clj-bytes.impl
  (:require [clj-bytes.protocols :as proto])
  (:import java.util.Arrays
           java.nio.ByteBuffer
           java.util.Random
           java.security.SecureRandom))

(def empty-bytes
  "Static empty bytes."
  (byte-array 0))

(def bytes-class
  "Class of bytes."
  (type empty-bytes))

(def ^:dynamic ^Random *random*
  "Internal random object."
  (SecureRandom.))

(defn rand-bytes
  "Generate random n bytes."
  [n]
  (let [b (byte-array n)] (.nextBytes *random* b) b))

(defn- byte->uint
  "Convert byte to unsigned int."
  [i]
  (bit-and 0xff i))

(defn- uint->byte
  "Convert unsigned int to byte."
  [i]
  (unchecked-byte i))

(defn concat-bytes
  "Concat seq of bytes."
  [bs]
  (if (empty? bs)
    empty-bytes
    (let [os (->> bs (map alength) (reductions +))
          nb (Arrays/copyOf (bytes (first bs)) (last os))]
      (doseq [[o b] (zipmap (butlast os) (rest bs))]
        (System/arraycopy b 0 nb o (alength b)))
      nb)))

(defn- reverse-bytes
  "Reverse bytes."
  [b]
  (->> b reverse byte-array))

(defmacro define-from-int-convertor
  {:doc "Construct -from-int convertor function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name length byte-buffer-setter]
  `(defn- ~name
     [^long ~'i]
     (let [~'b (byte-array ~length)]
       (-> ~'b ByteBuffer/wrap (~byte-buffer-setter ~'i))
       ~'b)))

(defmacro define-from-uint-convertor
  {:doc "Construct -from-uint convertor function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name from-int-convertor unchecked-cast]
  `(defn- ~name
     [^long ~'i]
     (-> ~'i ~unchecked-cast ~from-int-convertor)))

(defmacro define-from-int-le-convertor
  {:doc "Construct -from-int-le convertor function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name from-int-convertor]
  `(defn- ~name
     [^long ~'i]
     (-> ~'i ~from-int-convertor reverse-bytes)))

(defmacro define-to-int-convertor
  {:doc "Construct -to-int convert function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name length byte-buffer-getter]
  `(defn- ~name
     [^bytes ~'b]
     (assert (= (alength ~'b) ~length))
     (~byte-buffer-getter (ByteBuffer/wrap ~'b))))

(defmacro define-to-uint-convertor
  {:doc "Construct to-uint convert function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name to-int-convertor bit-mask]
  `(defn- ~name
     [^bytes ~'b]
     (-> (~to-int-convertor ~'b) (bit-and ~bit-mask))))

(defmacro define-to-int-le-convertor
  {:doc "Construct to-int-le convert function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name to-int-convertor]
  `(defn- ~name
     [^bytes ~'b]
     (-> ~'b reverse-bytes ~to-int-convertor)))

(defn- int8->bytes [i] (let [b (byte-array 1)] (-> b (aset-byte 0 i)) b))

(define-from-int-convertor int16->bytes 2 .putShort)
(define-from-int-convertor int32->bytes 4 .putInt)
(define-from-int-convertor int64->bytes 8 .putLong)

(define-from-uint-convertor uint8->bytes  int8->bytes  unchecked-byte)
(define-from-uint-convertor uint16->bytes int16->bytes unchecked-short)
(define-from-uint-convertor uint32->bytes int32->bytes unchecked-int)

(define-from-int-le-convertor int16-le->bytes  int16->bytes)
(define-from-int-le-convertor int32-le->bytes  int32->bytes)
(define-from-int-le-convertor int64-le->bytes  int64->bytes)
(define-from-int-le-convertor uint16-le->bytes uint16->bytes)
(define-from-int-le-convertor uint32-le->bytes uint32->bytes)

(defn- bytes->int8 [^bytes b] (assert (= (alength b) 1)) (aget b 0))

(define-to-int-convertor bytes->int16 2 .getShort)
(define-to-int-convertor bytes->int32 4 .getInt)
(define-to-int-convertor bytes->int64 8 .getLong)

(define-to-uint-convertor bytes->uint8  bytes->int8  0xff)
(define-to-uint-convertor bytes->uint16 bytes->int16 0xffff)
(define-to-uint-convertor bytes->uint32 bytes->int32 0xffffffff)

(define-to-int-le-convertor bytes->int16-le  bytes->int16)
(define-to-int-le-convertor bytes->int32-le  bytes->int32)
(define-to-int-le-convertor bytes->int64-le  bytes->int64)
(define-to-int-le-convertor bytes->uint16-le bytes->uint16)
(define-to-int-le-convertor bytes->uint32-le bytes->uint32)

(defn int->bytes
  "Convert int to bytes."
  [i encoding]
  (case encoding
    :int8      (int8->bytes      i)
    :uint8     (uint8->bytes     i)
    :int16-be  (int16->bytes     i)
    :int32-be  (int32->bytes     i)
    :int64-be  (int64->bytes     i)
    :uint16-be (uint16->bytes    i)
    :uint32-be (uint32->bytes    i)
    :int16-le  (int16-le->bytes  i)
    :int32-le  (int32-le->bytes  i)
    :int64-le  (int64-le->bytes  i)
    :uint16-le (uint16-le->bytes i)
    :uint32-le (uint32-le->bytes i)))

(defn bytes->int
  "Convert bytes to int."
  [b encoding]
  (case encoding
    :int8      (bytes->int8      b)
    :uint8     (bytes->uint8     b)
    :int16-be  (bytes->int16     b)
    :int32-be  (bytes->int32     b)
    :int64-be  (bytes->int64     b)
    :uint16-be (bytes->uint16    b)
    :uint32-be (bytes->uint32    b)
    :int16-le  (bytes->int16-le  b)
    :int32-le  (bytes->int32-le  b)
    :int64-le  (bytes->int64-le  b)
    :uint16-le (bytes->uint16-le b)
    :uint32-le (bytes->uint32-le b)))

(def impl
  (reify
    proto/BytesImpl
    (-instance? [_ b]
      (bytes? b))
    (-class [_]
      bytes-class)
    (-make [_ n]
      (byte-array (long n)))
    (-rand [_ n]
      (rand-bytes n))
    (-empty [_]
      empty-bytes)
    (-empty? [_ b]
      (zero? (alength b)))
    (-equal? [_ b1 b2]
      (Arrays/equals (bytes b1) (bytes b2)))
    (-count [_ b]
      (alength (bytes b)))
    (-get [_ b n]
      (aget (bytes b) n))
    (-set! [_ b n i]
      (aset-byte (bytes b) n i))
    (-fill! [_ b i]
      (Arrays/fill (bytes b) (byte i)))
    (-seq [_ b]
      (seq b))
    (-of-seq [_ s]
      (byte-array (seq s)))
    (-uget [_ b n]
      (-> (aget (bytes b) n) byte->uint))
    (-uset! [_ b n i]
      (aset-byte (bytes b) n (uint->byte i)))
    (-ufill! [_ b i]
      (Arrays/fill (bytes b) (byte (uint->byte i))))
    (-useq [_ b]
      (->> (seq b) (map byte->uint)))
    (-of-useq [_ s]
      (byte-array (seq s)))
    (-sub [_ b s e]
      (Arrays/copyOfRange (bytes b) s e))
    (-concat [_ bs]
      (concat-bytes bs))
    (-str [_ b encoding]
      (String. (bytes b) ^String encoding))
    (-of-str [_ s encoding]
      (.getBytes ^String s ^String encoding))
    (-int [_ b encoding]
      (bytes->int b encoding))
    (-of-int [_ i encoding]
      (int->bytes i encoding))))
