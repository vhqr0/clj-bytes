(ns clj-bytes.struct
  (:refer-clojure :exclude [keys byte short int long])
  (:require [clj-bytes.core :as b :refer [#?@(:cljs [byte-array])]]))

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
  ([st b]
   (unpack-one st b 0 (alength b)))
  ([st b s e]
   (let [[s d] (-unpack st b s e)]
     (if (= s e)
       d
       (unpack-not-eof-error)))))

(defn unpack-many
  ([st b]
   (unpack-many st b 0 (alength b)))
  ([st b s e]
   (loop [s s ds []]
     (if (= s e)
       ds
       (let [[s d] (-unpack st b s e)]
         (recur s (conj ds d)))))))

(defn pack-one
  [st d]
  (-pack st d))

(defn pack-many
  [st ds]
  (->> ds (map (partial pack-one st)) b/join))

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

(defn wrap-struct
  [bytes-st st]
  (wrap bytes-st (partial unpack-one st) (partial pack-one st)))

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

(defrecord FixedLengthFrameStruct [length content-struct]
  Struct
  (-unpack [_ b s e]
    (let [ns (+ s length)]
      (if (<= ns e)
        [ns (unpack-one content-struct b s ns)]
        (unpack-eof-error))))
  (-pack [_ d]
    (let [b (pack-one content-struct d)]
      (if (= (alength b) length)
        b
        (pack-invalid-data-error)))))

(defn ->st-frame-fixed
  ([c]
   (->st-frame-fixed c st-bytes))
  ([c content-st]
   (->FixedLengthFrameStruct c content-st)))

(defrecord VarLengthFrameStruct [length-struct content-struct]
  Struct
  (-unpack [_ b s e]
    (let [[s length] (-unpack length-struct b s e)
          ns (+ s length)]
      (if (<= ns e)
        [ns (unpack-one content-struct b s ns)]
        (unpack-eof-error))))
  (-pack [_ d]
    (let [b (pack-one content-struct d)
          length (alength b)]
      (b/join [(pack-one length-struct length) b]))))

(defn ->st-frame-var
  ([length-st]
   (->st-frame-var length-st st-bytes))
  ([length-st content-st]
   (->VarLengthFrameStruct length-st content-st)))

(defrecord DelimitedFrameStruct [delimiter content-struct]
  Struct
  (-unpack [_ b s e]
    (let [ns (b/index-of b s e delimiter)]
      (if-not (= ns -1)
        [(+ ns (alength delimiter)) (unpack-one content-struct b s ns)]
        (unpack-eof-error))))
  (-pack [_ d]
    (let [b (pack-one content-struct d)]
      (b/join [b delimiter]))))

(defn ->st-frame-delimited
  ([delim]
   (->st-frame-delimited delim st-bytes))
  ([delim content-st]
   (->DelimitedFrameStruct delim content-st)))

(defrecord StrStruct []
  Struct
  (-unpack [_ b s e]
    [e (b/get-str b s (- e s))])
  (-pack [_ s]
    (b/str->bytes s)))

(def st-str (->StrStruct))

(defn ->st-line
  [delim]
  (->st-frame-delimited (b/str->bytes delim) st-str))

(def st-unix-line (->st-line "\n"))
(def st-http-line (->st-line "\r\n"))

(defrecord FixedNumberStruct [get-fn put-fn length]
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

(def st-byte   (->FixedNumberStruct b/get-byte   b/put-byte   1))
(def st-short  (->FixedNumberStruct b/get-short  b/put-short  2))
(def st-int    (->FixedNumberStruct b/get-int    b/put-int    4))
(def st-long   (->FixedNumberStruct b/get-long   b/put-long   8))
(def st-ubyte  (->FixedNumberStruct b/get-ubyte  b/put-ubyte  1))
(def st-ushort (->FixedNumberStruct b/get-ushort b/put-ushort 2))
(def st-uint   (->FixedNumberStruct b/get-uint   b/put-uint   4))
(def st-float  (->FixedNumberStruct b/get-float  b/put-float  4))
(def st-double (->FixedNumberStruct b/get-double b/put-double 8))
