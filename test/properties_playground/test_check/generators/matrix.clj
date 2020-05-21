(ns properties-playground.test-check.generators.matrix
  (:require [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]))

;; row x column
;; 3x3 matrix
;; [[1 2 3]
;;  [4 5 6]
;;  [7 8 9]]
;; 3x2 matrix
;; [[1 2]
;;  [3 4]
;;  [5 6]]

;; not shrinkable
(defn gen-matrix [gen]
  (gen/let [[n m] (gen/tuple (gen/fmap inc gen/nat) (gen/fmap inc gen/nat))]
    (gen/vector
     (gen/vector gen m)
     n)))

;; shrinkable
(defn gen-matrix2 [gen]
  (gen/fmap
   (fn [[r c ns]]
     (mapv vec (take r (partition c (cycle ns)))))
   (gen/tuple (gen/fmap inc gen/nat)
              (gen/fmap inc gen/nat)
              (gen/not-empty (gen/vector gen 1000)))))

;; strategy 2: generate actions
;; [[:row [1 2 3 ...]] [:col [1 2 4 ...]]]
(defn gen-elements [gen]
  (gen/fmap
   cycle
   (gen/not-empty (gen/vector gen))))s

(defn gen-actions [gen]
  (gen/tuple (gen/elements [:row :col])
             (gen-elements gen)))

(defn process-action [matrix [action elements]]
  (cond
    (empty? matrix)
    [[(first elements)]]

    (= action :row)
    (let [columns (count (first matrix))]
      (conj matrix (vec (take columns elements))))

    (= action :col)
    (mapv conj matrix elements)))

(defn gen-matrix3 [gen]
  (gen/fmap
   (fn [actions]
     (reduce process-action [] actions))
   (gen/not-empty (gen/vector (gen-actions gen)))))

(comment
  (gen/sample (gen/vector (gen/vector gen/small-integer 3) 3))
  (gen/sample (gen/tuple (gen/elements [3]) (gen/elements [3])))
  (gen/sample (gen-matrix gen/double))
  (gen/sample (gen-matrix2 gen/nat))
  (process-action [[1 3] [2 4] [3 99]] [:row [8 2 3 4]])
  (gen/sample (gen-matrix3 gen/nat)))
