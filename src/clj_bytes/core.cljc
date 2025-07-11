(ns clj-bytes.core
  (:refer-clojure :exclude [rand])
  (:require #?@(:clj [[clj-bytes.clj.impl :as impl]])
            #?@(:cljs [[clj-bytes.cljs.impl :as impl]])))

#?(:cljs (do
           (def bytes?     impl/bytes?)
           (def byte-array impl/byte-array)
           (def aset-byte  impl/aset-byte)))

(def rand               impl/rand)
(def copy               impl/copy)
(def copy-of            impl/copy-of)
(def copy-of-range      impl/copy-of-range)
(def fill               impl/fill)
(def equals?            impl/equals?)
(def join               impl/join)
(def index-of           impl/index-of)
(def put-byte           impl/put-byte)
(def put-short          impl/put-short)
(def put-int            impl/put-int)
(def put-long           impl/put-long)
(def put-ubyte          impl/put-ubyte)
(def put-ushort         impl/put-ushort)
(def put-uint           impl/put-uint)
(def put-float          impl/put-float)
(def put-double         impl/put-double)
(def get-byte           impl/get-byte)
(def get-short          impl/get-short)
(def get-int            impl/get-int)
(def get-long           impl/get-long)
(def get-ubyte          impl/get-ubyte)
(def get-ushort         impl/get-ushort)
(def get-uint           impl/get-uint)
(def get-float          impl/get-float)
(def get-double         impl/get-double)
(def get-str            impl/get-str)
(def bytes->str         impl/bytes->str)
(def str->bytes         impl/str->bytes)
(def bytes->hex         impl/bytes->hex)
(def hex->bytes         impl/hex->bytes)
(def bytes->mime-base64 impl/bytes->mime-base64)
(def bytes->url-base64  impl/bytes->url-base64)
(def mime-base64->bytes impl/mime-base64->bytes)
(def url-base64->bytes  impl/url-base64->bytes)
