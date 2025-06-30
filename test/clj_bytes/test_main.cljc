(ns clj-bytes.test-main
  (:require [clojure.test :refer [run-tests]]
            clj-bytes.core2-test))

(defn -main
  [& _]
  (run-tests 'clj-bytes.core2-test))

#?(:cljs (-main))
