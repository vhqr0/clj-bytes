(ns clj-bytes.core
  (:refer-clojure :exclude [bytes? class empty empty? rand count get set! seq concat])
  (:require [clj-bytes.protocols :as proto]
            [clj-bytes.impl :as impl]))

(def ^:dynamic *impl*
  "Default implementation of bytes."
  impl/impl)

(defn bytes?
  "Pred whether the object a bytes."
  [b]
  (proto/-bytes? *impl* b))

(defn class
  "Return class of bytes object."
  []
  (proto/-class *impl*))

(defn empty
  "Return an empty bytes object."
  []
  (proto/-empty *impl*))

(defn empty?
  "Pred whether bytes is empty."
  [b]
  (proto/-empty? *impl* b))

(defn make
  "Return a bytes with length of n."
  [n]
  (proto/-make *impl* n))

(defn rand
  "Return a random bytes with length of n."
  [n]
  (proto/-rand *impl* n))

(defn seq->bytes
  "Return a bytes from seq of bytes."
  [s]
  (proto/-seq->bytes *impl* s))

^:rct/test
(comment
  (bytes? (empty)) ; => true
  )

(defn count
  "Return length of bytes."
  [b]
  (proto/-count *impl* b))

(defn get
  "Return nth byte of bytes."
  [b n]
  (proto/-get *impl* b n))

(defn set!
  "Set nth byte of bytes."
  [b n i]
  (proto/-set! *impl* b n i))

(defn fill!
  "Fill bytes."
  [b i]
  (proto/-fill! *impl* b i))

(defn seq
  "Return seq of bytes."
  [b]
  (proto/-seq *impl* b))

(defn uget
  "Unsigned version of `get`."
  [b n]
  (proto/-uget *impl* b n))

(defn uset!
  "Unsigned version of `set!`."
  [b n i]
  (proto/-uset! *impl* b n i))

(defn ufill!
  "Unsigned version of `fill!`."
  [b i]
  (proto/-ufill! *impl* b i))

(defn useq
  "Unsigned version of `seq`."
  [b]
  (proto/-useq *impl* b))

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

(defn str->bytes
  "Encode string."
  ([s]
   (str->bytes s "utf-8"))
  ([s encoding]
   (proto/-str->bytes *impl* s encoding)))

(defn bytes->str
  "Decode string."
  ([b]
   (bytes->str b "utf-8"))
  ([b encoding]
   (proto/-bytes->str *impl* b encoding)))

(defn int->bytes
  "Encode integer."
  [i encoding]
  (proto/-int->bytes *impl* i encoding))

(defn bytes->int
  "Decode integer."
  [b encoding]
  (proto/-bytes->int *impl* b encoding))
