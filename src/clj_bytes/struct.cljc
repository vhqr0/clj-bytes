(ns clj-bytes.struct
  (:refer-clojure :exclude [keys bytes int str])
  (:require [clj-bytes.core :as b]))

(defmulti pack
  "Input data and struct, return packed bytes."
  (fn [_d st] (:type st)))

(defmulti unpack
  "Input bytes and struct, return unpacked data and remain bytes, or nil if wait for more bytes."
  (fn [_b st] (:type st)))

(defmulti expand
  "Expand struct to lower level struct."
  (fn [st] (:type st)))

;; expand struct and pack/unpack again by default
(defmethod pack :default [d st] (pack d (expand st)))
(defmethod unpack :default [b st] (unpack b (expand st)))

(defn throw-struct-error
  "Throw struct related error."
  ([]
   (throw-struct-error "struct error"))
  ([msg]
   (throw (ex-info msg {:reason :struct-error}))))

(defn unpack-or-throw
  "Unpack one data, throw if wait for more bytes."
  [b st]
  (or (unpack b st) (throw-struct-error "must unpack at least one data")))

(defn unpack-one
  "Unpack one data."
  [b st]
  (let [[d b] (unpack-or-throw b st)]
    (if (b/empty? b)
      d
      (throw-struct-error "must unpack at most one data"))))

(defn unpack-many
  "Unpack many data."
  [b st]
  (loop [ds [] b b]
    (if (b/empty? b)
      ds
      (let [[d b] (unpack-or-throw b st)]
        (recur (conj ds d) b)))))

;;; combinators

;;;; wrap

(defmethod pack :wrap [d {:keys [struct pack-fn]}]
  (pack (pack-fn d) struct))

(defmethod unpack :wrap [b {:keys [struct unpack-fn]}]
  (when-let [[d b] (unpack b struct)]
    [(unpack-fn d) b]))

(defn wrap
  "Wrap struct by pack-fn and unpack-fn."
  [st pack-fn unpack-fn]
  {:type :wrap
   :pack-fn pack-fn
   :unpack-fn unpack-fn
   :struct st})

(defn wrap-struct
  "Wrap bytes-to-bytes struct by another struct."
  [st wrap-st]
  (-> st
      (wrap #(pack % wrap-st) #(unpack % wrap-st))))

;;;; tuple

(defmethod pack :tuple [ds {:keys [structs]}]
  (if-not (= (count ds) (count structs))
    (throw-struct-error "must pack specified num of data")
    (->> (map pack ds structs) (apply b/concat!))))

(defmethod unpack :tuple [b {:keys [structs]}]
  (loop [ds [] b b sts structs]
    (if (empty? sts)
      [ds b]
      (when-let [[d b] (unpack b (first sts))]
        (recur (conj ds d) b (rest sts))))))

(defn tuple
  "Construct tuple struct."
  [& structs]
  {:type :tuple
   :structs structs})

^:rct/test
(comment
  (-> [0x1 0x3456 0x4]
      (pack (tuple uint8 uint16-be uint8))
      (b/equal? (b/of-seq [0x1 0x34 0x56 0x4])))
  ;; => true
  (-> (b/of-seq [0x1 0x34 0x56 0x4])
      (unpack (tuple uint8 uint16-be uint8))
      first)
  ;; => [0x1 0x3456 0x4]
  )

;;;; keys

(defmethod pack :keys [m {:keys [key-structs]}]
  (->> key-structs
       (map
        (fn [[k st]]
          (pack (get m k) st)))
       (apply b/concat!)))

(defmethod unpack :keys [b {:keys [key-structs]}]
  (loop [m {} b b ksts key-structs]
    (if (empty? ksts)
      [m b]
      (let [[k st] (first ksts)]
        (when-let [[v b] (unpack b st)]
          (recur (assoc m k v) b (rest ksts)))))))

(defn keys
  "Construct keys struct."
  [& kvs]
  {:type :keys
   :key-structs (->> kvs (partition 2))})

;;;; key-fns

(defmethod pack :key-fns [m {:keys [key-struct-fns]}]
  (->> key-struct-fns
       (map
        (fn [[k st-fn]]
          (pack (get m k) (st-fn m))))
       (apply b/concat!)))

(defmethod unpack :key-fns [b {:keys [key-struct-fns]}]
  (loop [m {} b b kstfns key-struct-fns]
    (if (empty? kstfns)
      [m b]
      (let [[k st-fn] (first kstfns)]
        (when-let [[v b] (unpack b (st-fn m))]
          (recur (assoc m k v) b (rest kstfns)))))))

(defn key-fns
  "Construct contextual keys struct.
  Similar to `keys`, but struct of each keys are contextual,
  calculate by context when need."
  [& kvs]
  {:type :key-fns
   :key-struct-fns (->> kvs (partition 2))})

;;; primitives

;;;; bytes

(defmethod pack :bytes [d _st] d)
(defmethod unpack :bytes [b _st] [b (b/empty)])

(def bytes
  "Bytes struct."
  {:type :bytes})

;;;; bytes-fixed

(defmethod pack :bytes-fixed [d {:keys [length]}]
  (if (= (b/count d) length)
    d
    (throw-struct-error "must pack specified length of bytes")))

(defmethod unpack :bytes-fixed [b {:keys [length]}]
  (when (<= length (b/count b))
    (b/split-at! length b)))

(defn bytes-fixed
  "Construct fixed length bytes struct."
  [n]
  {:type :bytes-fixed :length n})

^:rct/test
(comment
  (unpack (b/of-seq [1 2 3 4 5]) (bytes-fixed 2))
  ;; =>> [#(b/equal? % (b/of-seq [1 2])) #(b/equal? (b/of-seq [3 4 5]) %)]
  )

;;;; bytes-delimited

(defmethod pack :bytes-delimited [d {:keys [delimiter]}]
  (b/concat d delimiter))

(defmethod unpack :bytes-delimited [b {:keys [delimiter]}]
  (when-let [i (b/index-of b delimiter)]
    (let [d (b/sub! b 0 i)
          b (b/sub! b (+ i (b/count delimiter)) (b/count b))]
      [d b])))

(defn bytes-delimited
  "Construct delimiterd bytes struct."
  [delimiter]
  {:type :bytes-delimited :delimiter delimiter})

^:rct/test
(comment
  (-> (b/of-str "hello")
      (pack (bytes-delimited (b/of-str "\r\n")))
      (b/equal? (b/of-str "hello\r\n")))
  ;; => true
  (-> (b/of-str "hello\r\nworld")
      (unpack (bytes-delimited (b/of-str "\r\n")))
      second
      (b/equal? (b/of-str "world")))
  ;; => true
  )

;;;; bytes-var

(defmethod expand :bytes-var [{:keys [length-struct]}]
  (-> (key-fns
       :length (constantly length-struct)
       :content (comp bytes-fixed :length))
      (wrap (fn [content] {:length (b/count content) :content content})
            :content)))

(defn bytes-var
  "Construct variable length bytes struct."
  [st]
  {:type :bytes-var :length-struct st})

^:rct/test
(comment
  (-> (b/of-seq [1 2 3])
      (pack (bytes-var uint8))
      (b/equal? (b/of-seq [3 1 2 3])))
  ;; => true
  (-> (b/of-seq [3 1 2 3])
      (unpack (bytes-var uint8))
      first
      (b/equal? (b/of-seq [1 2 3])))
  ;; => true
  )

;;;; int

(def int-encoding->bytes-length
  "Mapping from int encoding to bytes length."
  {:int8      1
   :uint8     1
   :int16-be  2
   :int32-be  4
   :int64-be  8
   :uint16-be 2
   :uint32-be 4
   :int16-le  2
   :int32-le  4
   :int64-le  8
   :uint16-le 2
   :uint32-le 4})

(def bytes-length->uint-be-encoding
  "Mapping from bytes length to uint be encoding."
  {1 :uint8 2 :uint16-be 4 :uint32-be})

(defn wrap-int
  "Wrap bytes-to-bytes struct by int."
  [st encoding]
  (-> st
      (wrap
       #(b/of-int % encoding)
       #(b/int % encoding))))

(defmethod expand :int [{:keys [encoding]}]
  (let [c (int-encoding->bytes-length encoding)]
    (-> (bytes-fixed c)
        (wrap-int encoding))))

(defn int
  "Construct int struct."
  [encoding]
  {:type :int :encoding encoding})

(def int8      (int :int8))
(def uint8     (int :uint8))
(def int16-be  (int :int16-be))
(def int32-be  (int :int32-be))
(def int64-be  (int :int64-be))
(def uint16-be (int :uint16-be))
(def uint32-be (int :uint32-be))
(def int16-le  (int :int16-le))
(def int32-le  (int :int32-le))
(def int64-le  (int :int64-le))
(def uint16-le (int :uint16-le))
(def uint32-le (int :uint32-le))

;;;; bits

(defn bits-lengths->bytes-length
  "Mapping from bits lengths to bytes length."
  [cs]
  (let [c (->> cs (reduce +))]
    (assert (zero? (mod c 8)))
    (bit-shift-right c 3)))

(defn bits-lengths->int-offsets
  "Mapping from bits lengths to int offsets."
  [cs]
  (->> cs reverse (reductions + 0) butlast reverse vec))

(defn bits-lengths->int-masks
  "Mapping from bits lengths to int masks."
  [cs]
  (->> cs (mapv #(dec (bit-shift-left 1 %)))))

^:rct/test
(comment
  (bits-lengths->bytes-length [2 4 3 4 3]) ; => 2
  (bits-lengths->int-offsets [2 4 3 4 3]) ; => [14 10 7 3 0]
  (bits-lengths->int-masks [2 4 3 4 3]) ; => [3 15 7 15 7]
  )

(defn bits->int
  "Convert bits to int."
  [bits offsets]
  (reduce + (map bit-shift-left bits offsets)))

(defn int->bits
  "Convert int to bits."
  [i offsets masks]
  (-> (fn [offset mask]
        (-> i (bit-shift-right offset) (bit-and mask)))
      (mapv offsets masks)))

(defn bits
  "Construct bits struct."
  [cs]
  (let [encoding (-> cs bits-lengths->bytes-length bytes-length->uint-be-encoding)
        offsets (bits-lengths->int-offsets cs)
        masks (bits-lengths->int-masks cs)]
    (-> (int encoding)
        (wrap
         #(bits->int % offsets)
         #(int->bits % offsets masks)))))

^:rct/test
(comment
  (-> [2 9 1]
      (pack (bits [2 4 2]))
      (b/equal? (b/of-seq [0xa5])))
  ;; => true
  (-> (b/of-seq [0xa5])
      (unpack (bits [2 4 2]))
      first)
  ;; => [2 9 1]
  )

;;;; str

(defn wrap-str
  "Wrap bytes-to-bytes struct by str."
  ([st]
   (wrap-str st nil))
  ([st encoding]
   (-> st
       (wrap
        #(b/of-str % encoding)
        #(b/str % encoding)))))

(defmethod expand :str [{:keys [encoding]}]
  (-> bytes
      (wrap-str encoding)))

(def str
  "String struct."
  {:type :str})

(defmethod expand :line [{:keys [encoding end]}]
  (-> (bytes-delimited (b/of-str end encoding))
      (wrap-str encoding)))

(defn line
  "Consruct line struct."
  ([]
   (line "\r\n"))
  ([end]
   {:type :line :end end}))

(def unix-line "Unix line that end with \n." (line "\n"))
(def http-line "Http line that end with \r\n." (line "\r\n"))
