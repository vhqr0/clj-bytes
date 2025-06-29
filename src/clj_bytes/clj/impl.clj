(ns clj-bytes.clj.impl
  (:refer-clojure :exclude [rand])
  (:import [java.util Arrays Random HexFormat Base64]
           [java.nio ByteBuffer]
           [java.security SecureRandom]
           [clj_bytes.clj.java Utils]))

(def bytes-class (class (byte-array 0)))

(def ^:dynamic ^Random *random* (SecureRandom.))

(defn rand
  ^bytes [n]
  (let [b (byte-array n)]
    (.nextBytes *random* b)
    b))

(defn copy
  ^bytes [s ss d ds c]
  (System/arraycopy (bytes s) (int ss) (bytes d) (int ds) (int c)))

(defn copy-of
  ^bytes [b c]
  (Arrays/copyOf (bytes b) (int c)))

(defn copy-of-range
  ^bytes [b s e]
  (Arrays/copyOfRange (bytes b) (int s) (int e)))

(defn fill
  ([b v]
   (Arrays/fill (bytes b) (byte v)))
  ([b s e v]
   (Arrays/fill (bytes b) (int s) (int e) (byte v))))

(defn equals?
  ([x y]
   (Arrays/equals (bytes x) (bytes y)))
  ([x xs xe y ys ye]
   (Arrays/equals (bytes x) (int xs) (int xe) (bytes y) (int ys) (int ye))))

(defn index-of
  ([b sep]
   (Utils/indexOfBytes (bytes b) (bytes sep)))
  ([b s e sep]
   (Utils/indexOfBytes (bytes b) (int s) (int e) (bytes sep))))

(defn join
  ([bs]
   (Utils/joinBytesArray (into-array bytes-class bs)))
  ([sep bs]
   (join (interpose sep bs))))

(defn put-byte [b s i] (aset-byte b s (byte i)))
(defn get-byte [b s] (aget b s))

(defn put-short  [b s i] (.putShort  (ByteBuffer/wrap (bytes b)) (int s) (short  i)))
(defn put-int    [b s i] (.putInt    (ByteBuffer/wrap (bytes b)) (int s) (int    i)))
(defn put-long   [b s i] (.putLong   (ByteBuffer/wrap (bytes b)) (int s) (long   i)))
(defn put-float  [b s f] (.putFloat  (ByteBuffer/wrap (bytes b)) (int s) (float  f)))
(defn put-double [b s f] (.putDouble (ByteBuffer/wrap (bytes b)) (int s) (double f)))

(defn get-short  [b s] (.getShort  (ByteBuffer/wrap (bytes b)) (int s)))
(defn get-int    [b s] (.getInt    (ByteBuffer/wrap (bytes b)) (int s)))
(defn get-long   [b s] (.getLong   (ByteBuffer/wrap (bytes b)) (int s)))
(defn get-float  [b s] (.getFloat  (ByteBuffer/wrap (bytes b)) (int s)))
(defn get-double [b s] (.getDouble (ByteBuffer/wrap (bytes b)) (int s)))

(defn put-ubyte  [b s i] (put-byte  b s (unchecked-byte  i)))
(defn put-ushort [b s i] (put-short b s (unchecked-short i)))
(defn put-uint   [b s i] (put-int   b s (unchecked-int   i)))

(defn get-ubyte  [b s] (bit-and 0xff       (get-byte  b s)))
(defn get-ushort [b s] (bit-and 0xffff     (get-short b s)))
(defn get-uint   [b s] (bit-and 0xffffffff (get-int   b s)))

(defn bytes->str
  ^String [b]
  (String. (bytes b)))

(defn str->bytes
  ^bytes [^String s]
  (.getBytes s))

(defn bytes->hex
  ^String [b]
  (.formatHex (HexFormat/of) b))

(defn hex->bytes
  ^bytes [^String s]
  (.parseHex (HexFormat/of) s))

(defn bytes->mime-base64
  ^String [b]
  (String. (.encode (Base64/getMimeEncoder) (bytes b))))

(defn bytes->url-base64
  ^String [b]
  (String. (.encode (Base64/getUrlEncoder) (bytes b))))

(defn mime-base64->bytes
  ^bytes [^String s]
  (.decode (Base64/getMimeDecoder) s))

(defn url-base64->bytes
  ^bytes [^String s]
  (.decode (Base64/getUrlDecoder) s))
