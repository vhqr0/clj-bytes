(ns clj-bytes.core
  (:refer-clojure :exclude [instance? class empty empty? rand count get set! seq concat str int])
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
  (proto/-make *impl* n))

(defn rand
  "Return a random bytes with length of n."
  [n]
  (proto/-rand *impl* n))

(defn empty
  "Return an empty bytes object."
  []
  (proto/-empty *impl*))

(defn empty?
  "Pred whether bytes is empty."
  [b]
  (proto/-empty? *impl* b))

(defn equal?
  "Pred whether two bytes are equal."
  [b1 b2]
  (proto/-equal? *impl* b1 b2))

(defn count
  "Return length of bytes."
  [b]
  (proto/-count *impl* b))

^:rct/test
(comment
  (instance? (empty)) ; => true
  (empty? (empty)) ; => true
  (empty? (make 0)) ; => true
  (empty? (make 1)) ; => false
  (equal? (empty) (empty)) ; => true
  (count (make 4)) ; => 4
  (count (rand 4)) ; => 4
  (count (empty)) ; => 0
  )

(defn get
  "Return nth byte of bytes."
  [b n]
  (proto/-get *impl* b n))

(defn set!
  "Set nth byte of bytes."
  [b n i]
  (proto/-set! *impl* b n i)
  b)

(defn fill!
  "Fill bytes."
  [b i]
  (proto/-fill! *impl* b i)
  b)

(defn seq
  "Return seq of bytes."
  [b]
  (proto/-seq *impl* b))

(defn of-seq
  "Return a bytes from seq of bytes."
  [s]
  (proto/-of-seq *impl* s))

(defn uget
  "Unsigned version of `get`."
  [b n]
  (proto/-uget *impl* b n))

(defn uset!
  "Unsigned version of `set!`."
  [b n i]
  (proto/-uset! *impl* b n i)
  b)

(defn ufill!
  "Unsigned version of `fill!`."
  [b i]
  (proto/-ufill! *impl* b i)
  b)

(defn useq
  "Unsigned version of `seq`."
  [b]
  (proto/-useq *impl* b))

(defn of-useq
  "Unsigned version of `of-seq`."
  [s]
  (proto/-of-useq *impl* s))

^:rct/test
(comment
  (-> (of-useq [255 256]) count) ; => 2
  (-> (of-useq [255 256]) (uget 0)) ; => 255
  (-> (of-useq [255 256]) (uget 1)) ; => 0
  (-> (of-useq [0]) (uset! 0 257) (uget 0)) ; => 1
  (-> (of-useq [0 1]) (ufill! 257) (uget 0)) ; => 1
  )

(defn sub
  "Return sub-bytes of bytes."
  ([b]
   (sub b 0 (count b)))
  ([b s e]
   (proto/-sub *impl* b s e)))

(defn concat
  "Return concatance of bytes."
  [& bs]
  (apply proto/-concat *impl* bs))

(defn sub!
  "Impure version of `sub`, that means try to reuse input bytes,
  and caller have to make sure the input bytes is readonly."
  [b s e]
  (if (and (= s 0) (= e (count b)))
    b
    (sub b s e)))

(defn concat!
  "Impure version of `concat`, that means try to reuse input bytes,
  and caller have to make sure the input bytes are readonly."
  [& bs]
  (let [bs (->> bs (remove empty?))]
    (cond (clojure.core/empty? bs) (empty)
          (clojure.core/empty? (rest bs)) (first bs)
          :else (apply concat bs))))

(defn str
  "Decode string."
  ([b]
   (str b "utf-8"))
  ([b encoding]
   (proto/-str *impl* b encoding)))

(defn of-str
  "Encode string."
  ([s]
   (of-str s "utf-8"))
  ([s encoding]
   (proto/-of-str *impl* s encoding)))

(defn int
  "Decode integer."
  [b encoding]
  (proto/-int *impl* b encoding))

(defn of-int
  "Encode integer."
  [i encoding]
  (proto/-of-int *impl* i encoding))

(defn hex
  "Encode hex string."
  ([b]
   (hex b :lower))
  ([b alphabet]
   (->> b useq (codec/ints->hex alphabet))))

(defn of-hex
  "Decode hex string."
  ([s]
   (of-hex s :lower))
  ([s alphabet]
   (-> s (codec/hex->ints alphabet) of-useq)))

(defn base64
  "Encode base64 string."
  ([b]
   (base64 b :mime))
  ([b alphabet]
   (->> b useq (codec/ints->base64 alphabet))))

(defn of-base64
  "Decode base64 string."
  ([s]
   (of-base64 s :mime))
  ([s alphabet]
   (-> s (codec/base64->ints alphabet) of-useq)))
