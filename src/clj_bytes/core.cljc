(ns clj-bytes.core
  (:refer-clojure :exclude [instance? class rand empty empty? count get set! seq concat str int])
  (:require [clj-bytes.protocols :as proto]
            [clj-bytes.impl :as impl]
            [clj-bytes.codec :as codec]))

(def ^:dynamic *impl*
  "Default implementation of bytes."
  impl/impl)

(defn instance?
  "Pred whether the object a bytes."
  [b]
  (proto/-instance? *impl* b))

(defn class
  "Return class of bytes object."
  []
  (proto/-class *impl*))

(defn make
  "Return a bytes with length of n."
  [n]
  {:pre [(int? n)]}
  (proto/-make *impl* n))

(defn rand
  "Return a random bytes with length of n."
  [n]
  {:pre [(int? n)]}
  (proto/-rand *impl* n))

(defn empty
  "Return an empty bytes object."
  []
  (proto/-empty *impl*))

(defn empty?
  "Pred whether bytes is empty."
  [b]
  {:pre [(instance? b)]}
  (proto/-empty? *impl* b))

(defn count
  "Return length of bytes."
  [b]
  {:pre [(instance? b)]}
  (proto/-count *impl* b))

^:rct/test
(comment
  (instance? (empty)) ; => true
  (empty? (empty)) ; => true
  (empty? (make 0)) ; => true
  (empty? (make 1)) ; => false
  (count (make 4)) ; => 4
  (count (rand 4)) ; => 4
  (count (empty)) ; => 0
  )

(defn get
  "Return nth byte of bytes."
  [b n]
  {:pre [(instance? b) (int? n)]}
  (proto/-get *impl* b n))

(defn set!
  "Set nth byte of bytes."
  [b n i]
  {:pre [(instance? b) (int? n) (int? i)]}
  (proto/-set! *impl* b n i)
  b)

(defn fill!
  "Fill bytes."
  [b i]
  {:pre [(instance? b) (int? i)]}
  (proto/-fill! *impl* b i)
  b)

(defn seq
  "Return seq of bytes."
  [b]
  {:pre [(instance? b)]}
  (proto/-seq *impl* b))

(defn of-seq
  "Return a bytes from seq of bytes."
  [s]
  {:post [(instance? %)]}
  (proto/-of-seq *impl* s))

(defn uget
  "Unsigned version of `get`."
  [b n]
  {:pre [(instance? b) (int? n)]}
  (proto/-uget *impl* b n))

(defn uset!
  "Unsigned version of `set!`."
  [b n i]
  {:pre [(instance? b) (int? n) (int? i)]}
  (proto/-uset! *impl* b n i)
  b)

(defn ufill!
  "Unsigned version of `fill!`."
  [b i]
  {:pre [(instance? b) (int? i)]}
  (proto/-ufill! *impl* b i)
  b)

(defn useq
  "Unsigned version of `seq`."
  [b]
  {:pre [(instance? b)]}
  (proto/-useq *impl* b))

(defn of-useq
  "Unsigned version of `of-seq`."
  [s]
  {:post [(instance? %)]}
  (proto/-of-useq *impl* s))

^:rct/test
(comment
  (-> (of-useq [255 256]) count) ; => 2
  (-> (of-useq [255 256]) (uget 0)) ; => 255
  (-> (of-useq [255 256]) (uget 1)) ; => 0
  (-> (of-useq [0]) (uset! 0 257) (uget 0)) ; => 1
  (-> (of-useq [0 1]) (ufill! 257) (uget 0)) ; => 1
  )

(defn equal?
  "Pred whether two bytes are equal."
  ([b1 b2]
   (equal? b1 0 (count b1) b2 0 (count b2)))
  ([b1 s1 e1 b2 s2 e2]
   {:pre [(<= 0 s1 e1 (count b1)) (<= 0 s2 e2 (count b2))]}
   (proto/-equal? *impl* b1 s1 e1 b2 s2 e2)))

(defn index-of
  "Find needle in haystack, return index of needle, or nil if not found."
  ([h n]
   (index-of h n 0 (count h)))
  ([h n s e]
   {:pre [(<= 0 s e (count h)) (instance? n)]}
   (proto/-index-of *impl* h n s e)))

^:rct/test
(comment
  (equal? (empty) (empty)) ; => true
  (equal? (empty) (make 1)) ; => false
  (index-of (of-seq [1 2 3 4 5]) (of-seq [3 4])) ; => 2
  (index-of (of-seq [1 2 3 4 5]) (of-seq [4])) ; => 3
  (index-of (of-seq [1 2 3 4 5]) (of-seq [4 3])) ; => nil
  )

(defn sub
  "Return sub-bytes of bytes."
  ([b]
   (sub b 0 (count b)))
  ([b s e]
   {:pre [(<= 0 s e (count b))]}
   (proto/-sub *impl* b s e)))

(defn join
  "Return concatance of seq of bytes."
  ([bs]
   (proto/-join *impl* bs))
  ([sep bs]
   (join (interpose sep bs))))

(defn concat
  "Return concatance of bytes."
  [& bs]
  (join bs))

(defn sub!
  "Impure version of `sub`, that means try to reuse input bytes,
  and caller have to make sure the input bytes is readonly."
  ([b]
   (sub! b 0 (count b)))
  ([b s e]
   (if (= s e)
     (empty)
     (if (and (= s 0) (= e (count b)))
       b
       (sub b s e)))))

(defn join!
  "Impure version of `join`, that means try to reuse input bytes,
  and caller have to make sure the input bytes are readonly."
  ([bs]
   (let [bs (->> bs (remove empty?))]
     (cond (clojure.core/empty? bs) (empty)
           (clojure.core/empty? (rest bs)) (first bs)
           :else (join bs))))
  ([sep bs]
   (join! (interpose sep bs))))

(defn concat!
  "Impure version of `concat`, that means try to reuse input bytes,
  and caller have to make sure the input bytes are readonly."
  [& bs]
  (join! bs))

(defn split-at!
  "Split bytes at n."
  [n b]
  [(sub! b 0 n) (sub! b n (count b))])

^:rct/test
(comment
  (equal? (concat! (of-seq [1 2 3])) (of-seq [1 2 3])) ; => true
  (equal? (concat! (of-seq [1 2 3]) (of-seq [4 5 6])) (of-seq [1 2 3 4 5 6])) ; => true
  (equal? (concat! (empty) (of-seq [1 2 3]) (empty)) (of-seq [1 2 3])) ; => true
  (equal? (concat! (of-seq [1 2 3]) (empty) (of-seq [4 5 6])) (of-seq [1 2 3 4 5 6])) ; => true
  (equal? (sub! (of-seq [1 2 3 4 5]) 1 3) (of-seq [2 3])) ; => true
  (equal? (sub! (of-seq [1 2 3 4 5]) 1 4) (of-seq [2 3 4])) ; => true
  (equal? (sub! (of-seq [1 2 3 4 5]) 1 5) (of-seq [2 3 4 5])) ; => true
  (equal? (sub! (of-seq [1 2 3 4 5])) (of-seq [1 2 3 4 5])) ; => true
  (equal? (first (split-at! 2 (of-seq [1 2 3 4 5]))) (of-seq [1 2])) ; => true
  (equal? (second (split-at! 2 (of-seq [1 2 3 4 5]))) (of-seq [3 4 5])) ; => true
  )

(defn str
  "Decode string."
  ([b]
   (str b nil))
  ([b encoding]
   {:pre [(instance? b)]
    :post [(string? %)]}
   (let [encoding (or encoding "utf-8")]
     (proto/-str *impl* b encoding))))

(defn of-str
  "Encode string."
  ([s]
   (of-str s nil))
  ([s encoding]
   {:pre [(string? s)]
    :post [(instance? %)]}
   (let [encoding (or encoding "utf-8")]
     (proto/-of-str *impl* s encoding))))

^:rct/test
(comment
  (equal? (of-str "hello") (of-seq [104 101 108 108 111])) ; => true
  (equal? (concat (of-str "hello, ") (of-str "world")) (of-str "hello, world")) ; => true
  )

(defn int
  "Decode integer."
  [b encoding]
  {:pre [(instance? b)]
   :post [(int? %)]}
  (proto/-int *impl* b encoding))

(defn of-int
  "Encode integer."
  [i encoding]
  {:pre [(int? i)]
   :post [(instance? %)]}
  (proto/-of-int *impl* i encoding))

^:rct/test
(comment
  (equal? (of-int 0x1234 :uint16-be) (of-seq [0x12 0x34])) ; => true
  (equal? (of-int 0x1234 :uint16-le) (of-seq [0x34 0x12])) ; => true
  (int (of-seq [0x12 0x34]) :uint16-be) ; => 0x1234
  )

(defn hex
  "Encode hex string."
  ([b]
   (hex b :lower))
  ([b alphabet]
   {:pre [(instance? b)]
    :post [(string? %)]}
   (->> b useq (codec/ints->hex alphabet))))

(defn of-hex
  "Decode hex string."
  ([s]
   (of-hex s :lower))
  ([s alphabet]
   {:pre [(string? s)]
    :post [(instance? %)]}
   (-> s (codec/hex->ints alphabet) of-useq)))

^:rct/test
(comment
  (hex (of-seq [0x12 0x34])) ; => "1234"
  (hex (of-seq [0x1a 0x2b])) ; => "1a2b"
  (hex (of-seq [0x1a 0x2b]) :upper) ; => "1A2B"
  (equal? (of-hex "1a2b") (of-seq [0x1a 0x2b])) ; => true
  )

(defn base64
  "Encode base64 string."
  ([b]
   (base64 b :mime))
  ([b alphabet]
   {:pre [(instance? b)]
    :post [(string? %)]}
   (->> b useq (codec/ints->base64 alphabet))))

(defn of-base64
  "Decode base64 string."
  ([s]
   (of-base64 s :mime))
  ([s alphabet]
   {:pre [(string? s)]
    :post [(instance? %)]}
   (-> s (codec/base64->ints alphabet) of-useq)))

^:rct/test
(comment
  (base64 (of-str "hello")) ; => "aGVsbG8="
  (str (of-base64 "aGVsbG8=")) ; => "hello"

  (def wiki-test-text
    "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure.")
  (def wiki-test-base64
    "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")

  (= (base64 (of-str wiki-test-text)) wiki-test-base64) ; => true
  (= (str (of-base64 wiki-test-base64)) wiki-test-text) ; => true
  )
