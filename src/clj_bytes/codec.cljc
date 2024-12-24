(ns clj-bytes.codec
  (:require [clojure.string :as str]
            [clj-bytes.core :as b]))

;;; utils

(defn reverse-vec-map
  [v]
  (->> v (map-indexed #(vector %2 %1)) (into {})))

(defn reverse-alphabet
  [m]
  (->> m (map #(update % 1 reverse-vec-map)) (into {})))

;;; hex

(def int->hex-char
  {:lower (vec "0123456789abcdef")
   :upper (vec "0123456789ABCDEF")})

(def hex-char->int (reverse-alphabet int->hex-char))

(defn int->hex-chars
  [i int->char]
  (->> [(bit-shift-right i 4) (bit-and 0xf i)]
       (map int->char)))

(defn ints->hex-chars
  [ints int->char]
  (->> ints (mapcat #(int->hex-chars % int->char))))

(defn hex-chars->int
  [chars char->int]
  (assert (= (count chars) 2))
  (let [[i1 i2] (map char->int chars)]
    (+ (bit-shift-left i1 4) i2)))

(defn hex-chars->ints
  [chars char->int]
  (->> chars (partition-all 2) (map #(hex-chars->int % char->int))))

(defn bytes->hex
  ([b]
   (bytes->hex b :lower))
  ([b alphebet]
   (let [int->char (int->hex-char alphebet)]
     (-> b b/useq (ints->hex-chars int->char) str/join))))

(defn hex->bytes
  ([s]
   (hex->bytes s :lower))
  ([s alphabet]
   (let [char->int (hex-char->int alphabet)]
     (-> s seq (hex-chars->ints char->int) b/seq->bytes))))

;;; base64
;; https://zh.wikipedia.org/zh-cn/Base64

(def int->base64-char
  {:mime (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
   :url  (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")})

(def base64-char->int
  (reverse-alphabet int->base64-char))

(defn ints-part->base64-chars-3
  [[i1 i2 i3] int->char]
  (->> [(bit-shift-right i1 2)
        (+ (bit-shift-left (bit-and 0x3 i1) 4) (bit-shift-right (bit-and 0xf0 i2) 4))
        (+ (bit-shift-left (bit-and 0xf i2) 2) (bit-shift-right (bit-and 0xc0 i3) 6))
        (bit-and 0x3f i3)]
       (map int->char)))

(defn ints-part->base64-chars-2
  [[i1 i2] int->char]
  (concat
   (->> [(bit-shift-right i1 2)
         (+ (bit-shift-left (bit-and 0x3 i1) 4) (bit-shift-right (bit-and 0xf0 i2) 4))
         (bit-shift-left (bit-and 0xf i2) 2)]
        (map int->char))
   [\=]))

(defn ints-part->base64-chars-1
  [[i1] int->char]
  (concat
   (->> [(bit-shift-right i1 2)
         (bit-shift-left (bit-and 0x3 i1) 4)]
        (map int->char))
   [\= \=]))

(defn ints-part->base64-chars
  [ints int->char]
  (case (count ints)
    1 [true (ints-part->base64-chars-1 ints int->char)]
    2 [true (ints-part->base64-chars-2 ints int->char)]
    3 [false (ints-part->base64-chars-3 ints int->char)]))

(defn ints->base64-chars
  [ints int->char]
  (lazy-seq
   (when (seq ints)
     (let [[ints-part ints] (split-at 3 ints)
           [end? chars] (ints-part->base64-chars ints-part int->char)]
       (when end?
         (assert (empty? ints)))
       (concat chars (ints->base64-chars ints int->char))))))

(defn base64-chars-part->ints-4
  [[i1 i2 i3 i4]]
  [(+ (bit-shift-left i1 2) (bit-shift-right i2 4))
   (+ (bit-shift-left i2 4) (bit-shift-right i3 2))
   (+ (bit-shift-left i3 6) i4)])

(defn base64-chars-part->ints-3
  [[i1 i2 i3]]
  [(+ (bit-shift-left i1 2) (bit-shift-right i2 4))
   (+ (bit-shift-left i2 4) (bit-shift-right i3 2))])

(defn base64-chars-part->ints-2
  [[i1 i2]]
  [(+ (bit-shift-left i1 2) (bit-shift-right i2 4))])

(defn base64-chars-part->ints
  [chars char->int]
  (assert (= (count chars) 4))
  (let [ints (->> chars (take-while #(not= % \=)) (map char->int))]
    (case (count ints)
      2 [true (base64-chars-part->ints-2 ints)]
      3 [true (base64-chars-part->ints-3 ints)]
      4 [false (base64-chars-part->ints-4 ints)])))

(defn base64-chars->ints
  [chars char->int]
  (lazy-seq
   (when (seq chars)
     (let [[chars-part chars] (split-at 4 chars)
           [end? ints] (base64-chars-part->ints chars-part char->int)]
       (when end?
         (assert (empty? chars)))
       (concat ints (base64-chars->ints chars char->int))))))

(defn bytes->base64
  ([b]
   (bytes->base64 b :mime))
  ([b alphabet]
   (let [int->char (int->base64-char alphabet)]
     (-> b b/useq (ints->base64-chars int->char) str/join))))

(defn base64->bytes
  ([s]
   (base64->bytes s :mime))
  ([s alphabet]
   (let [char->int (base64-char->int alphabet)]
     (-> s seq (base64-chars->ints char->int) b/seq->bytes))))
