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

(def ^:dynamic ^Random *random* (SecureRandom.))

(defn rand-bytes
  [n]
  (let [b (byte-array n)]
    (.nextBytes *random* b)
    b))

(defn byte->uint
  "Convert byte to unsigned int."
  [i]
  (bit-and 0xff i))

(defn uint->byte
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

(defn reverse-bytes
  "Reverse bytes."
  [b]
  (->> b reverse byte-array))

(defn int8->bytes
  [i]
  (let [b (byte-array 1)]
    (-> b (aset-byte 0 i))
    b))

(defmacro define-from-int-convertor
  {:doc "Construct -from-int convertor function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name length byte-buffer-setter]
  `(defn ~name
     [^long ~'i]
     (let [~'b (byte-array ~length)]
       (-> ~'b ByteBuffer/wrap (~byte-buffer-setter ~'i))
       ~'b)))

(define-from-int-convertor int16->bytes 2 .putShort)
(define-from-int-convertor int32->bytes 4 .putInt)
(define-from-int-convertor int64->bytes 8 .putLong)

(defmacro define-from-uint-convertor
  {:doc "Construct -from-uint convertor function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name from-int-convertor unchecked-cast]
  `(defn ~name
     [^long ~'i]
     (-> ~'i ~unchecked-cast ~from-int-convertor)))

(define-from-uint-convertor uint8->bytes  int8->bytes  unchecked-byte)
(define-from-uint-convertor uint16->bytes int16->bytes unchecked-short)
(define-from-uint-convertor uint32->bytes int32->bytes unchecked-int)

(defmacro define-from-int-le-convertor
  {:doc "Construct -from-int-le convertor function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name from-int-convertor]
  `(defn ~name
     [^long ~'i]
     (-> ~'i ~from-int-convertor reverse-bytes)))

(define-from-int-le-convertor int16-le->bytes  int16->bytes)
(define-from-int-le-convertor int32-le->bytes  int32->bytes)
(define-from-int-le-convertor int64-le->bytes  int64->bytes)
(define-from-int-le-convertor uint16-le->bytes uint16->bytes)
(define-from-int-le-convertor uint32-le->bytes uint32->bytes)

(defn encoding->from-int-fn
  "Mapping from encoding to -from-int function."
  [encoding]
  (case encoding
    :int8      int8->bytes
    :uint8     uint8->bytes
    :int16-be  int16->bytes
    :int32-be  int32->bytes
    :int64-be  int64->bytes
    :uint16-be uint16->bytes
    :uint32-be uint32->bytes
    :int16-le  int16-le->bytes
    :int32-le  int32-le->bytes
    :int64-le  int64-le->bytes
    :uint16-le uint16-le->bytes
    :uint32-le uint32-le->bytes))

(defn int->bytes
  "Convert int to bytes."
  [i encoding]
  (let [from-int-fn (encoding->from-int-fn encoding)]
    (from-int-fn i)))

(defn bytes->int8
  [^bytes b]
  (assert (= (alength b) 1))
  (aget b 0))

(defmacro define-to-int-convertor
  {:doc "Construct -to-int convert function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name length byte-buffer-getter]
  `(defn ~name
     [^bytes ~'b]
     (assert (= (alength ~'b) ~length))
     (~byte-buffer-getter (ByteBuffer/wrap ~'b))))

(define-to-int-convertor bytes->int16 2 .getShort)
(define-to-int-convertor bytes->int32 4 .getInt)
(define-to-int-convertor bytes->int64 8 .getLong)

(defmacro define-to-uint-convertor
  {:doc "Construct to-uint convert function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name to-int-convertor bit-mask]
  `(defn ~name
     [^bytes ~'b]
     (-> (~to-int-convertor ~'b) (bit-and ~bit-mask))))

(define-to-uint-convertor bytes->uint8  bytes->int8  0xff)
(define-to-uint-convertor bytes->uint16 bytes->int16 0xffff)
(define-to-uint-convertor bytes->uint32 bytes->int32 0xffffffff)

(defmacro define-to-int-le-convertor
  {:doc "Construct to-int-le convert function."
   :clj-kondo/lint-as 'clojure.core/def}
  [name to-int-convertor]
  `(defn ~name
     [^bytes ~'b]
     (-> ~'b reverse-bytes ~to-int-convertor)))

(define-to-int-le-convertor bytes->int16-le  bytes->int16)
(define-to-int-le-convertor bytes->int32-le  bytes->int32)
(define-to-int-le-convertor bytes->int64-le  bytes->int64)
(define-to-int-le-convertor bytes->uint16-le bytes->uint16)
(define-to-int-le-convertor bytes->uint32-le bytes->uint32)

(defn encoding->to-int-fn
  "Mapping from encoding to -to-int function."
  [encoding]
  (case encoding
    :int8      bytes->int8
    :uint8     bytes->uint8
    :int16-be  bytes->int16
    :int32-be  bytes->int32
    :int64-be  bytes->int64
    :uint16-be bytes->uint16
    :uint32-be bytes->uint32
    :int16-le  bytes->int16-le
    :int32-le  bytes->int32-le
    :int64-le  bytes->int64-le
    :uint16-le bytes->uint16-le
    :uint32-le bytes->uint32-le))

(defn bytes->int
  "Convert bytes to int."
  [b encoding]
  (let [to-int-fn (encoding->to-int-fn encoding)]
    (assert (some? to-int-fn))
    (to-int-fn b)))

(def impl
  (reify
    proto/BytesImpl
    (-bytes? [_ b]
      (bytes? b))
    (-class [_]
      bytes-class)
    (-empty [_]
      empty-bytes)
    (-empty? [_ b]
      (zero? (alength b)))
    (-make [_ n]
      (byte-array (long n)))
    (-rand [_ n]
      (rand-bytes n))
    (-seq->bytes [_ s]
      (byte-array (seq s)))
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
    (-uget [_ b n]
      (-> (aget (bytes b) n) byte->uint))
    (-uset! [_ b n i]
      (aset-byte (bytes b) n (uint->byte i)))
    (-ufill! [_ b i]
      (Arrays/fill (bytes b) (byte (uint->byte i))))
    (-useq [_ b]
      (->> (seq b) (map byte->uint)))
    (-sub [_ b s e]
      (Arrays/copyOfRange (bytes b) s e))
    (-concat [_ bs]
      (concat-bytes bs))
    (-str->bytes [_ s encoding]
      (.getBytes ^String s ^String encoding))
    (-bytes->str [_ b encoding]
      (String. (bytes b) ^String encoding))
    (-int->bytes [_ i encoding]
      (int->bytes i encoding))
    (-bytes->int [_ b encoding]
      (bytes->int b encoding))))
