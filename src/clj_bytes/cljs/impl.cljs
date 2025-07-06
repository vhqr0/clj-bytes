(ns clj-bytes.cljs.impl
  (:refer-clojure :exclude [rand])
  (:require [goog.crypt :as crypt]
            [goog.crypt.base64 :as base64]))

;;; cljs compat

;; cljs core related fn: `byte` `unchecked-byte` `bytes` `aset` `aget` `alength` `seq`
;; clj core extra related fn: `bytes?` `byte-array` `aset-byte`

(defn bytes?
  [x]
  (instance? js/Int8Array x))

(defn byte-array
  ([size-or-seq]
   (if (int? size-or-seq)
     (js/Int8Array. size-or-seq)
     (js/Int8Array.from size-or-seq)))
  ([size init-val-or-seq]
   (let [b (js/Int8Array. size)]
     (if (int? init-val-or-seq)
       (.fill b init-val-or-seq)
       (.set b (js/Int8Array.from (take size init-val-or-seq)) 0))
     b)))

(defn aset-byte
  ([array idx val]
   (aset (bytes array) idx (byte val)))
  ([array idx idx2 & idxv]
   (apply aset-byte (aget array idx) idx2 idxv)))

;;; utils

(defn rand
  [n]
  (let [b (byte-array n)]
    (js/crypto.getRandomValues b)
    b))

(defn copy
  [s ss d ds c]
  (let [s (.subarray s ss (+ ss c))]
    (.set d s ds)))

(defn copy-of
  [b c]
  (let [b (.subarray b 0 (min c (alength b)))
        nb (byte-array c)]
    (.set nb b 0)
    nb))

(defn copy-of-range
  [b s e]
  (let [b (.subarray b s (min e (alength b)))
        nb (byte-array (- e s))]
    (.set nb b 0)
    nb))

(defn fill
  ([b v]
   (.fill b v))
  ([b s e v]
   (.fill b v s e)))

(defn equals?
  ([x y]
   (equals? x 0 (alength x) y 0 (alength y)))
  ([x xs xe y ys ye]
   (let [x (.subarray x xs xe)
         y (.subarray y ys ye)
         xc (alength x)
         yc (alength y)]
     (and (= xc yc)
          (loop [i 0]
            (if (= i xc)
              true
              (if-not (= (aget x i) (aget y i))
                false
                (recur (inc i)))))))))

(defn join
  ([bs]
   (let [nb (byte-array (->> bs (map alength) (reduce +)))]
     (loop [o 0 bs bs]
       (if (empty? bs)
         nb
         (let [b (first bs)
               c (alength b)]
           (copy b 0 nb o c)
           (recur (+ o c) (rest bs)))))))
  ([sep bs]
   (join (interpose sep bs))))

(defn index-of
  ([b sep]
   (index-of b 0 (alength b) sep))
  ([b s e sep]
   (let [c (alength sep)
         e (- e c)]
     (loop [i s]
       (if (> i e)
         -1
         (if (equals? b i (+ i c) sep 0 c)
           i
           (recur (inc i))))))))

(defn bytes->dataview
  [b]
  (js/DataView. (.-buffer b) (.-byteOffset b)))

(defn put-byte   [b s i] (.setInt8    (bytes->dataview b) s i))
(defn put-short  [b s i] (.setInt16   (bytes->dataview b) s i))
(defn put-int    [b s i] (.setInt32   (bytes->dataview b) s i))
(defn put-ubyte  [b s i] (.setUint8   (bytes->dataview b) s i))
(defn put-ushort [b s i] (.setUint16  (bytes->dataview b) s i))
(defn put-uint   [b s i] (.setUint32  (bytes->dataview b) s i))
(defn put-float  [b s f] (.setFloat32 (bytes->dataview b) s f))
(defn put-double [b s f] (.setFloat64 (bytes->dataview b) s f))

(defn get-byte   [b s] (.getInt8    (bytes->dataview b) s))
(defn get-short  [b s] (.getInt16   (bytes->dataview b) s))
(defn get-int    [b s] (.getInt32   (bytes->dataview b) s))
(defn get-ubyte  [b s] (.getUint8   (bytes->dataview b) s))
(defn get-ushort [b s] (.getUint16  (bytes->dataview b) s))
(defn get-uint   [b s] (.getUint32  (bytes->dataview b) s))
(defn get-float  [b s] (.getFloat32 (bytes->dataview b) s))
(defn get-double [b s] (.getFloat64 (bytes->dataview b) s))

(defn put-long [_b _s _i] (throw (ex-info "Runtime doesn't support put-long" {})))
(defn get-long [_b _s] (throw (ex-info "Runtime doesn't support get-long" {})))

(defn bytes->uint8-array
  [b]
  (js/Uint8Array. (.-buffer b) (.-byteOffset b)))

(defn uint8-array->bytes
  [a]
  (js/Int8Array. (.-buffer a) (.-byteOffset a)))

(defn get-str
  [b s c]
  (.decode (js/TextDecoder.) (.subarray b s (+ s c))))

(defn bytes->str
  [b]
  (.decode (js/TextDecoder.) b))

(defn str->bytes
  [s]
  (uint8-array->bytes (.encode (js/TextEncoder.) s)))

(defn bytes->hex
  [b]
  (crypt/byteArrayToHex (bytes->uint8-array b)))

(defn hex->bytes
  [s]
  (js/Int8Array.from (crypt/hexToByteArray s)))

(defn bytes->mime-base64
  [b]
  (base64/encodeByteArray (bytes->uint8-array b) base64/Alphabet.DEFAULT))

(defn bytes->url-base64
  [b]
  (base64/encodeByteArray (bytes->uint8-array b) base64/Alphabet.WEBSAFE))

(defn mime-base64->bytes
  [s]
  (uint8-array->bytes (base64/decodeStringToUint8Array s)))

(defn url-base64->bytes
  [s]
  (uint8-array->bytes (.-buffer (base64/decodeStringToUint8Array s))))
