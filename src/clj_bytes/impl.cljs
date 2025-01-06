(ns clj-bytes.impl
  (:refer-clojure :exclude [seq])
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

(defn seq
  [b]
  (array-seq (js/Int8Array. b)))

(defn of-seq
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

(defn useq
  [b]
  (array-seq (js/Uint8Array. b)))

(defn of-useq
  [s]
  (.-buffer (js/Uint8Array.from s)))

(defn bytes-equal?
  [b1 b2]
  (and (= (bytes-count b1) (bytes-count b2))
       (->> (map vector (useq b1) (useq b2))
            (every? (fn [[i1 i2]] (= i1 i2))))))

(defn bytes-concat
  [bs]
  (let [os (->> bs (map bytes-concat) (reductions +))
        na (js/Uint8Array. (last os))]
    (doseq [[o b] (map vector (cons 0 (butlast os)) bs)]
      (.set na (js/Uint8Array. b) o))
    na.buffer))

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
      (seq b))
    (-of-seq [_ s]
      (of-seq s))
    (-uget [_ b n]
      (bytes-uget b n))
    (-uset! [_ b n i]
      (bytes-uset! b n i))
    (-ufill! [_ b i]
      (bytes-ufill! b i))
    (-useq [_ b]
      (useq b))
    (-of-useq [_ s]
      (of-useq s))
    (-sub [_ b s e]
      (.slice b s e))
    (-concat [_ bs]
      (bytes-concat bs))
    (-str [_ b encoding]
      (-> (js/TextDecoder. encoding) (.decode b)))
    (-of-str [_ s encoding]
      (-> (js/TextEncoder. encoding) (.encode s)))))
