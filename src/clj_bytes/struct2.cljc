(ns clj-bytes.struct2
  (:refer-clojure :exclude [keys byte short int long])
  (:require [clj-bytes.core2 :as b :refer [#?@(:cljs [byte-array])]]))

(defprotocol Struct
  (-unpack [this b s e]
    "Unpack a data in bytes.
Input: bytes b, start index s, end index e;
Output: new start index s, unpacked data d.")
  (-pack [this d]
    "Pack a data to bytes.
Input: data d;
Output: packed bytes b."))

(defn unpack-eof-error
  []
  (throw (ex-info "Unpack at EOF" {})))

(defn unpack-not-eof-error
  []
  (throw (ex-info "Unpack not at EOF" {})))

(defn pack-invalid-data-error
  []
  (throw (ex-info "Pack invalid data" {})))

(defn unpack-one
  [st b]
  (let [e (alength b)
        [s d] (-unpack st b 0 e)]
    (if (= s e)
      d
      (unpack-not-eof-error))))

(defn unpack-many
  [st b]
  (let [e (alength b)]
    (loop [s 0 ds []]
      (if (= s e)
        ds
        (let [[s d] (-unpack st b s e)]
          (recur s (conj ds d)))))))

(defrecord WrapStruct [struct unpack-fn pack-fn]
  Struct
  (-unpack [_ b s e]
    (let [[s d] (-unpack struct b s e)]
      [s (unpack-fn d)]))
  (-pack [_ d]
    (-pack struct (pack-fn d))))

(defn wrap
  [st unpack-fn pack-fn]
  (->WrapStruct st unpack-fn pack-fn))

(defrecord CollStruct [struct]
  Struct
  (-unpack [_ b s e]
    (loop [s s ds []]
      (if (= s e)
        [e ds]
        (let [[s d] (-unpack struct b s e)]
          (recur s (conj ds d))))))
  (-pack [_ ds]
    (->> ds (map (partial -pack struct)) b/join)))

(defn coll
  [st]
  (->CollStruct st))

(defrecord TupleStruct [structs]
  Struct
  (-unpack [_ b s e]
    (let [s s ds [] structs structs]
      (if (empty? structs)
        [s ds]
        (let [struct (first structs)
              [s d] (-unpack struct b s e)]
          (recur s (conj ds d) (rest structs))))))
  (-pack [_ ds]
    (->> (map -pack structs ds) b/join)))

(defn tuple
  [& sts]
  (->TupleStruct (vec sts)))

(defrecord KeysStruct [key-structs]
  Struct
  (-unpack [_ b s e]
    (loop [s s ds {} key-structs key-structs]
      (if (empty? key-structs)
        [s ds]
        (let [[key struct] (first key-structs)
              [s d] (-unpack struct b s e)]
          (recur s (assoc ds key d) (rest key-structs))))))
  (-pack [_ ds]
    (->> key-structs
         (map
          (fn [[key struct]]
            (-pack struct (get ds key))))
         b/join)))

(defn keys
  [& ksts]
  (->KeysStruct (->> ksts (partition 2) vec)))

(defrecord BytesStruct []
  Struct
  (-unpack [_ b s e]
    [e (b/copy-of-range b s e)])
  (-pack [_ b]
    b))

(def st-bytes (->BytesStruct))

(defrecord FixedLengthBytesStruct [length]
  Struct
  (-unpack [_ b s e]
    (let [ns (+ s length)]
      (if (<= ns e)
        [ns (b/copy-of-range b s ns)]
        (unpack-eof-error))))
  (-pack [_ b]
    (if (= (alength b) length)
      b
      (pack-invalid-data-error))))

(defn ->st-bytes-fixed
  [length]
  (->FixedLengthBytesStruct length))

(defn unpack-bytes-fixed
  [length b s e]
  (-unpack (->st-bytes-fixed length) b s e))

(defrecord VarLengthBytesStruct [length-struct]
  Struct
  (-unpack [_ b s e]
    (let [[s length] (-unpack length-struct b s e)
          ns (+ s length)]
      (if (<= ns e)
        [ns (b/copy-of-range b s ns)]
        (unpack-eof-error))))
  (-pack [_ b]
    (b/join [(-pack length-struct (alength b)) b])))

(defn ->st-bytes-var
  [st-length]
  (->VarLengthBytesStruct st-length))

(defrecord DelimitedBytesStruct [delimiter]
  Struct
  (-unpack [_ b s e]
    (let [ns (b/index-of b s e delimiter)]
      (if-not (= ns -1)
        [(+ ns (alength delimiter)) (b/copy-of-range b s ns)]
        (unpack-eof-error))))
  (-pack [_ d]
    (b/join [d delimiter])))

(defn ->st-bytes-delimited
  [delimiter]
  (->DelimitedBytesStruct delimiter))

(defrecord StrStruct [bytes-struct]
  Struct
  (-unpack [_ b s e]
    (let [[s b] (-unpack bytes-struct b s e)]
      [s (b/bytes->str b)]))
  (-pack [_ s]
    (-pack bytes-struct (b/str->bytes s))))

(def st-str (->StrStruct st-bytes))

(defn ->st-line
  [delimiter]
  (let [st-bytes (->st-bytes-delimited (b/str->bytes delimiter))]
    (->StrStruct st-bytes)))

(def st-unix-line (->st-line "\n"))
(def st-http-line (->st-line "\r\n"))

(defrecord IntStruct [get-fn put-fn length]
  Struct
  (-unpack [_ b s e]
    (let [ns (+ s length)]
      (if (<= ns e)
        [ns (get-fn b s)]
        (unpack-eof-error))))
  (-pack [_ i]
    (let [b (byte-array length)]
      (put-fn b 0 i)
      b)))

(def st-byte   (->IntStruct b/get-byte   b/put-byte   1))
(def st-short  (->IntStruct b/get-short  b/put-short  2))
(def st-int    (->IntStruct b/get-int    b/put-int    4))
(def st-long   (->IntStruct b/get-long   b/put-long   8))
(def st-ubyte  (->IntStruct b/get-ubyte  b/put-ubyte  1))
(def st-ushort (->IntStruct b/get-ushort b/put-ushort 2))
(def st-uint   (->IntStruct b/get-uint   b/put-uint   4))
