(ns clj-bytes.codec
  (:require [clojure.string :as str]))

;;; utils

(defn- reverse-alphabet-1
  "Reverse a single alphabet, from int->char to char->int."
  [v]
  (->> v (map-indexed #(vector %2 %1)) (into {})))

^:rct/test
(comment
  (reverse-alphabet-1 [\a \b \c]) ; => {\a 0 \b 1 \c 2}
  )

(defn- reverse-alphabet
  "Reverse alphabet, mapping from keyword to single alphabet."
  [m]
  (->> m (map #(update % 1 reverse-alphabet-1)) (into {})))

^:rct/test
(comment
  (reverse-alphabet {:lower [\a \b \c] :upper [\A \B \C]})
  ;; => {:lower {\a 0 \b 1 \c 2} :upper {\A 0 \B 1 \C 2}}
  )

;;; hex

(def ^:private int->hex-char
  {:lower (vec "0123456789abcdef")
   :upper (vec "0123456789ABCDEF")})

(def ^:private hex-char->int
  (reverse-alphabet int->hex-char))

(defn- int->hex-chars-part
  [i int->char]
  (->> [(bit-shift-right i 4) (bit-and 0xf i)] (map int->char)))

(defn- hex-chars-part->int
  [chars char->int]
  {:pre [(= (count chars) 2)]}
  (let [[i1 i2] (map char->int chars)]
    (+ (bit-shift-left i1 4) i2)))

(defn- ints->hex-chars
  [int->char ints]
  (->> ints (mapcat #(int->hex-chars-part % int->char))))

(defn- hex-chars->ints
  [char->int chars]
  (->> chars (partition-all 2) (map #(hex-chars-part->int % char->int))))

(defn- ints->hex-str
  [int->char ints]
  {:pre [(some? int->char)]}
  (->> ints (ints->hex-chars int->char) str/join))

(defn- hex-str->ints
  [s char->int]
  {:pre [(some? char->int)]}
  (->> (seq s) (hex-chars->ints char->int)))

(defn ints->hex
  "Convert ints to hex."
  [alphabet ints]
  (->> ints (ints->hex-str (get int->hex-char alphabet))))

(defn hex->ints
  "Convert hex to ints."
  [s alphabet]
  (-> s (hex-str->ints (get hex-char->int alphabet))))

;;; base64
;; https://zh.wikipedia.org/zh-cn/Base64

(def ^:private int->base64-char
  {:mime (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
   :url  (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")})

(def ^:private base64-char->int
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
  {:pre [(= (count chars) 4)]}
  (let [ints (->> chars (take-while #(not= % \=)) (map char->int))]
    (case (count ints)
      2 [true (base64-chars-part->ints-part-2 ints)]
      3 [true (base64-chars-part->ints-part-3 ints)]
      4 [false (base64-chars-part->ints-part-4 ints)])))

(defn- ints->base64-chars
  ([int->char ints]
   (ints->base64-chars int->char ints false))
  ([int->char ints end?]
   {:pre [(not (and end? (seq ints)))]}
   (lazy-seq
    (when (seq ints)
      (let [[ints-part ints] (split-at 3 ints)
            [end? chars] (ints-part->base64-chars-part ints-part int->char)]
        (concat chars (ints->base64-chars int->char ints end?)))))))

(defn- base64-chars->ints
  ([char->int chars]
   (base64-chars->ints char->int chars false))
  ([char->int chars end?]
   {:pre [(not (and end? (seq chars)))]}
   (lazy-seq
    (when (seq chars)
      (let [[chars-part chars] (split-at 4 chars)
            [end? ints] (base64-chars-part->ints-part chars-part char->int)]
        (concat ints (base64-chars->ints char->int chars end?)))))))

(defn- ints->base64-str
  [int->char ints]
  {:pre [(some? int->char)]}
  (->> ints (ints->base64-chars int->char) str/join))

(defn- base64-str->ints
  [s char->int]
  {:pre [(some? char->int)]}
  (->> (seq s) (base64-chars->ints char->int)))

(defn ints->base64
  "Convert ints to base64."
  [alphabet ints]
  (->> ints (ints->base64-str (get int->base64-char alphabet))))

(defn base64->ints
  "Convert base64 to ints."
  [s alphabet]
  (-> s (base64-str->ints (get base64-char->int alphabet))))
