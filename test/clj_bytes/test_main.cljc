(ns clj-bytes.test-main
  (:require [clojure.test :refer [run-tests]]
            clj-bytes.core-test))

(defn -main
  [& _]
  (run-tests 'clj-bytes.core-test))

#?(:cljs (-main))
