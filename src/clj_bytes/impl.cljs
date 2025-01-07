(ns clj-bytes.impl
  (:require [clj-bytes.protocols :as proto]))

(def byte-class
  "Static empty bytes."
  js/ArrayBuffer)

(defn make-bytes
  [n]
  (js/ArrayBuffer. n))

(defn rand-bytes
  [n]
  (-> (js/Uint8Array. n) js/crypto.getRandomValues .-buffer))

(defn bytes-count
  [b]
  b.byteLength)

(defn bytes-get
  [b n]
  (aget (js/Int8Array. b) n))

(defn bytes-set!
  [b n i]
  (aset (js/Int8Array. b) n i))

(defn bytes-fill!
  [b i]
  (.fill (js/Int8Array. b) i))

(defn bytes->seq
  [b]
  (array-seq (js/Int8Array. b)))

(defn seq->bytes
  [s]
  (.-buffer (js/Int8Array.from s)))

(defn bytes-uget
  [b n]
  (aget (js/Uint8Array. b) n))

(defn bytes-uset!
  [b n i]
  (aset (js/Uint8Array. b) n i))

(defn bytes-ufill!
  [b i]
  (.fill (js/Uint8Array. b) i))

(defn bytes->useq
  [b]
  (array-seq (js/Uint8Array. b)))

(defn useq->bytes
  [s]
  (.-buffer (js/Uint8Array.from s)))

(defn bytes-equal?
  [b1 b2]
  (and (= (bytes-count b1) (bytes-count b2))
       (->> (map vector (bytes->useq b1) (bytes->useq b2))
            (every? (fn [[i1 i2]] (= i1 i2))))))

(defn bytes-concat
  [bs]
  (let [os (->> bs (map bytes-concat) (reductions +))
        na (js/Uint8Array. (last os))]
    (doseq [[o b] (map vector (cons 0 (butlast os)) bs)]
      (.set na (js/Uint8Array. b) o))
    na.buffer))

(def int8->bytes  (fn [i] (.-buffer (js/Int8Array.of i))))
(def uint8->bytes (fn [i] (.-buffer (js/Uint8Array.of i))))

(def int16-be->bytes  (fn [i] (let [b (js/ArrayBuffer. 2)] (-> (js/DataView. b) (.setInt16 0 i false)) b)))
(def int32-be->bytes  (fn [i] (let [b (js/ArrayBuffer. 4)] (-> (js/DataView. b) (.setInt32 0 i false)) b)))
(def uint16-be->bytes (fn [i] (let [b (js/ArrayBuffer. 2)] (-> (js/DataView. b) (.setUint16 0 i false)) b)))
(def uint32-be->bytes (fn [i] (let [b (js/ArrayBuffer. 4)] (-> (js/DataView. b) (.setUint32 0 i false)) b)))
(def int16-le->bytes  (fn [i] (let [b (js/ArrayBuffer. 2)] (-> (js/DataView. b) (.setInt16 0 i true)) b)))
(def int32-le->bytes  (fn [i] (let [b (js/ArrayBuffer. 4)] (-> (js/DataView. b) (.setInt32 0 i true)) b)))
(def uint16-le->bytes (fn [i] (let [b (js/ArrayBuffer. 2)] (-> (js/DataView. b) (.setUint16 0 i true)) b)))
(def uint32-le->bytes (fn [i] (let [b (js/ArrayBuffer. 4)] (-> (js/DataView. b) (.setUint32 0 i true)) b)))

(def bytes->int8  (fn [b] (assert (= b.byteLength 1)) (aget (js/Int8Array. b) 0)))
(def bytes->uint8 (fn [b] (assert (= b.byteLength 1)) (aget (js/Uint8Array. b) 0)))

(def bytes->int16-be  (fn [b] (assert (= b.byteLength 2)) (-> (js/DataView. b) (.getInt16 0 false))))
(def bytes->int32-be  (fn [b] (assert (= b.byteLength 4)) (-> (js/DataView. b) (.getInt32 0 false))))
(def bytes->uint16-be (fn [b] (assert (= b.byteLength 2)) (-> (js/DataView. b) (.getUint16 0 false))))
(def bytes->uint32-be (fn [b] (assert (= b.byteLength 4)) (-> (js/DataView. b) (.getUint32 0 false))))
(def bytes->int16-le  (fn [b] (assert (= b.byteLength 2)) (-> (js/DataView. b) (.getInt16 0 true))))
(def bytes->int32-le  (fn [b] (assert (= b.byteLength 4)) (-> (js/DataView. b) (.getInt32 0 true))))
(def bytes->uint16-le (fn [b] (assert (= b.byteLength 2)) (-> (js/DataView. b) (.getUint16 0 true))))
(def bytes->uint32-le (fn [b] (assert (= b.byteLength 4)) (-> (js/DataView. b) (.getUint32 0 true))))

(defn int->bytes
  "Convert int to bytes."
  [i encoding]
  (case encoding
    :int8      (int8->bytes      i)
    :uint8     (uint8->bytes     i)
    :int16-be  (int16-be->bytes  i)
    :int32-be  (int32-be->bytes  i)
    :uint16-be (uint16-be->bytes i)
    :uint32-be (uint32-be->bytes i)
    :int16-le  (int16-le->bytes  i)
    :int32-le  (int32-le->bytes  i)
    :uint16-le (uint16-le->bytes i)
    :uint32-le (uint32-le->bytes i)))

(defn bytes->int
  "Convert bytes to int."
  [b encoding]
  (case encoding
    :int8      (bytes->int8      b)
    :uint8     (bytes->uint8     b)
    :int16-be  (bytes->int16-be  b)
    :int32-be  (bytes->int32-be  b)
    :uint16-be (bytes->uint16-be b)
    :uint32-be (bytes->uint32-be b)
    :int16-le  (bytes->int16-le  b)
    :int32-le  (bytes->int32-le  b)
    :uint16-le (bytes->uint16-le b)
    :uint32-le (bytes->uint32-le b)))

(def impl
  (reify
    proto/BytesImpl
    (-instance? [_ b]
      (instance? byte-class b))
    (-class [_]
      byte-class)
    (-make [_ n]
      (make-bytes n))
    (-rand [_ n]
      (rand-bytes n))
    (-empty [_]
      (make-bytes 0))
    (-empty? [_ b]
      (-> b bytes-count zero?))
    (-equal? [_ b1 b2]
      (bytes-equal? b1 b2))
    (-count [_ b]
      (bytes-count b))
    (-get [_ b n]
      (bytes-get b n))
    (-set! [_ b n i]
      (bytes-set! b n i))
    (-fill! [_ b i]
      (bytes-fill! b i))
    (-seq [_ b]
      (bytes->seq b))
    (-of-seq [_ s]
      (seq->bytes s))
    (-uget [_ b n]
      (bytes-uget b n))
    (-uset! [_ b n i]
      (bytes-uset! b n i))
    (-ufill! [_ b i]
      (bytes-ufill! b i))
    (-useq [_ b]
      (bytes->useq b))
    (-of-useq [_ s]
      (useq->bytes s))
    (-sub [_ b s e]
      (.slice b s e))
    (-concat [_ bs]
      (bytes-concat bs))
    (-str [_ b encoding]
      (-> (js/TextDecoder. encoding) (.decode b)))
    (-of-str [_ s encoding]
      (-> (js/TextEncoder. encoding) (.encode s)))
    (-int [_ b encoding]
      (bytes->int b encoding))
    (-of-int [_ i encoding]
      (int->bytes i encoding))))
