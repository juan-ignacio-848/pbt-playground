(ns properties-playground.test-check.generators.bank-account
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

;; {:id "dd7ec906-9958-11ea-94d2-e03f4978aaab"- --
;;  :balance 2394}

(gen/sample (gen/hash-map :id gen/uuid
                          :balance (gen/large-integer* {:min 0})))

(defn gen-movement []
  (gen/tuple (gen/elements [:deposit :withdrawal])
             (gen/large-integer* {:min 0})))

(defn apply-movements [balance [action amount]]
  (case action
    :deposit (+ balance amount)
    :withdrawal (- balance amount)))

(defn gen-balance []
  (gen/fmap
   (fn [movements]
     (reduce apply-movements 0 movements))
   (gen/not-empty (gen/vector (gen-movement)))))

(defn gen-account-balance []
  (gen/hash-map :id gen/uuid
                :balance (gen-balance)))
(gen/sample (gen-movement))
(gen/sample (gen-account-balance))
