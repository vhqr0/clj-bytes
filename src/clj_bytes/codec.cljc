(ns clj-bytes.codec
  (:require [clojure.string :as str]))

;;; utils

(defn reverse-alphabet-1
  "Reverse a single alphabet."
  [v]
  (->> v (map-indexed #(vector %2 %1)) (into {})))

^:rct/test
(comment
  (reverse-alphabet-1 [\a \b \c]) ; => {\a 0 \b 1 \c 2}
  )

(defn reverse-alphabet
  "Reverse alphabet."
  [m]
  (->> m (map #(update % 1 reverse-alphabet-1)) (into {})))

^:rct/test
(comment
  (reverse-alphabet {:lower [\a \b \c] :upper [\A \B \C]})
  ;; => {:lower {\a 0 \b 1 \c 2} :upper {\A 0 \B 1 \C 2}}
  )

;;; hex

(def int->hex-char
  "Hex int->char alphabet."
  {:lower (vec "0123456789abcdef")
   :upper (vec "0123456789ABCDEF")})

(def hex-char->int
  "Hex char->int alphabet."
  (reverse-alphabet int->hex-char))

(defn- int->hex-chars-part
  [i int->char]
  (->> [(bit-shift-right i 4) (bit-and 0xf i)] (map int->char)))

(defn- hex-chars-part->int
  [chars char->int]
  (assert (= (count chars) 2))
  (let [[i1 i2] (map char->int chars)]
    (+ (bit-shift-left i1 4) i2)))

(defn ints->hex-chars
  "Convert seq of ints to seq of hex chars."
  [int->char ints]
  (->> ints (mapcat #(int->hex-chars-part % int->char))))

(defn hex-chars->ints
  "Convert seq of hex chars to seq of ints."
  [char->int chars]
  (->> chars (partition-all 2) (map #(hex-chars-part->int % char->int))))

(defn ints->hex
  "Convert seq of ints to hex str."
  [alphabet ints]
  (let [int->char (int->hex-char alphabet)]
    (assert (some? int->char))
    (->> ints (ints->hex-chars int->char) str/join)))

(defn hex->ints
  "Convert hex str to seq of ints."
  [s alphabet]
  (let [char->int (hex-char->int alphabet)]
    (assert (some? char->int))
    (->> (seq s) (hex-chars->ints char->int))))

;;; base64
;; https://zh.wikipedia.org/zh-cn/Base64

(def int->base64-char
  "Base64 int->char alphabet."
  {:mime (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
   :url  (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")})

(def base64-char->int
  "Base64 char->int alphabet."
  (reverse-alphabet int->base64-char))

(defn- ints-part->base64-chars-part-3
  [[i1 i2 i3] int->char]
  (->> [(bit-shift-right i1 2)
        (+ (bit-shift-left (bit-and 0x3 i1) 4) (bit-shift-right (bit-and 0xf0 i2) 4))
        (+ (bit-shift-left (bit-and 0xf i2) 2) (bit-shift-right (bit-and 0xc0 i3) 6))
        (bit-and 0x3f i3)]
       (map int->char)))

(defn- ints-part->base64-chars-part-2
  [[i1 i2] int->char]
  (concat
   (->> [(bit-shift-right i1 2)
         (+ (bit-shift-left (bit-and 0x3 i1) 4) (bit-shift-right (bit-and 0xf0 i2) 4))
         (bit-shift-left (bit-and 0xf i2) 2)]
        (map int->char))
   [\=]))

(defn- ints-part->base64-chars-part-1
  [[i1] int->char]
  (concat
   (->> [(bit-shift-right i1 2)
         (bit-shift-left (bit-and 0x3 i1) 4)]
        (map int->char))
   [\= \=]))

(defn- ints-part->base64-chars-part
  [ints int->char]
  (case (count ints)
    1 [true (ints-part->base64-chars-part-1 ints int->char)]
    2 [true (ints-part->base64-chars-part-2 ints int->char)]
    3 [false (ints-part->base64-chars-part-3 ints int->char)]))

(defn- base64-chars-part->ints-part-4
  [[i1 i2 i3 i4]]
  [(+ (bit-shift-left i1 2) (bit-shift-right i2 4))
   (+ (bit-shift-left i2 4) (bit-shift-right i3 2))
   (+ (bit-shift-left i3 6) i4)])

(defn- base64-chars-part->ints-part-3
  [[i1 i2 i3]]
  [(+ (bit-shift-left i1 2) (bit-shift-right i2 4))
   (+ (bit-shift-left i2 4) (bit-shift-right i3 2))])

(defn- base64-chars-part->ints-part-2
  [[i1 i2]]
  [(+ (bit-shift-left i1 2) (bit-shift-right i2 4))])

(defn- base64-chars-part->ints-part
  [chars char->int]
  (assert (= (count chars) 4))
  (let [ints (->> chars (take-while #(not= % \=)) (map char->int))]
    (case (count ints)
      2 [true (base64-chars-part->ints-part-2 ints)]
      3 [true (base64-chars-part->ints-part-3 ints)]
      4 [false (base64-chars-part->ints-part-4 ints)])))

(defn ints->base64-chars
  "Convert seq of ints to seq of base64 chars."
  [int->char ints]
  (lazy-seq
   (when (seq ints)
     (let [[ints-part ints] (split-at 3 ints)
           [end? chars] (ints-part->base64-chars-part ints-part int->char)]
       (when end?
         (assert (empty? ints)))
       (concat chars (ints->base64-chars int->char ints))))))

(defn base64-chars->ints
  "Convert seq of base64 chars to seq of ints."
  [char->int chars]
  (lazy-seq
   (when (seq chars)
     (let [[chars-part chars] (split-at 4 chars)
           [end? ints] (base64-chars-part->ints-part chars-part char->int)]
       (when end?
         (assert (empty? chars)))
       (concat ints (base64-chars->ints char->int chars))))))

(defn ints->base64
  "Convert seq of ints to base64 str."
  [alphabet ints]
  (let [int->char (int->base64-char alphabet)]
    (assert (some? int->char))
    (->> ints (ints->base64-chars int->char) str/join)))

(defn base64->ints
  "Convert base64 str to seq of ints."
  [s alphabet]
  (let [char->int (base64-char->int alphabet)]
    (assert (some? char->int))
    (->> (seq s) (base64-chars->ints char->int))))
