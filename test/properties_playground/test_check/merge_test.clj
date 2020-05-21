(ns properties-playground.test-check.merge-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

(defspec merge-all-keys
  (prop/for-all [m1 (gen/map gen/keyword gen/small-integer)
                 m2 (gen/map gen/keyword gen/small-integer)]
                (= (set (concat (keys m1) (keys m2))) (set (keys (merge m1 m2))))))

(defspec merge-identity
  (prop/for-all [m1 (gen/map gen/keyword gen/small-integer)]
                (= m1
                   (merge m1 {})
                   (merge {} m1))))

(defspec merge-associativity
  (prop/for-all [m1 (gen/map gen/keyword gen/small-integer)
                 m2 (gen/map gen/keyword gen/small-integer)
                 m3 (gen/map gen/keyword gen/small-integer)]
                (= (merge m1 m2 m3) (merge m1 (merge m2 m3)))))

(defspec merge-on-duplicate-keys-rightmost-map-wins
  (prop/for-all [m1 (gen/map gen/keyword gen/small-integer)
                 m2 (gen/map gen/keyword gen/small-integer)
                 [k v1 v2] (gen/tuple gen/keyword gen/small-integer gen/small-integer)]
                (let [m1 (into m1 [[k v1]])
                      m2 (into m2 [[k v2]])]
                  (= (get (merge m1 m2) k) v2))))
(comment
  (merge {:a 2 :b 3} {:c 5 :a 3})
  (gen/sample (gen/map gen/keyword gen/small-integer))
  (gen/sample (gen/not-empty (gen/vector gen/keyword))))
