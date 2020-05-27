(ns properties-playground.test-check.stateful-test
  (:require  [clojure.test :as t]
             [clojure.test.check.clojure-test :refer [defspec]]
             [clojure.test.check.properties :as prop]
             [clojure.test.check.generators :as gen]
             [properties-playground.test-check.stateful :as sut]))

;; Given a score counter implemented with an atom
;; There are two operations available:
;; (add score x) being x a positive number
;; (value score) get current score

;; Property: score cannot decrease

;; counter operations: [:add 5] [:val]
(def gen-add-op (gen/tuple (gen/return :add) gen/small-integer))
(def gen-val-op (gen/tuple (gen/return :val)))
(def gen-counter-op (gen/one-of [gen-add-op
                                 gen-val-op]))
(def gen-score-ops (gen/not-empty (gen/vector gen-counter-op)))

(defn apply-score-op [counter [op arg]]
  (case op
    :add (sut/add counter arg)
    :val (sut/value counter)))

;; This proves that the score cannnot decrease.
;; It doesn't prove it actually increases.
;; Make the add function return always nil and this will still pass.
(defspec score-cannot-decrease 100
  (prop/for-all [ops gen-score-ops]
                (let [counter (sut/new-counter)]
                  (loop [ops ops]
                    (if (empty? ops)
                      true
                      (let [prev-val (sut/value counter)]
                        (try (apply-score-op counter (first ops))
                             (catch Throwable _))
                        (if (< (sut/value counter) prev-val)
                          false
                          (recur (rest ops)))))))))

;; To prove that add increases the score we will use a model.
;; In this case the model is as simple as a number.
(defn apply-score-model-op [model [op arg]]
  (case op
    :add (if (neg? arg)
           model
           (+ model arg))
    :val model))

(defspec score-model-test 100
  (prop/for-all [ops gen-score-ops]
                (let [counter (sut/new-counter)
                      model   (reduce apply-score-model-op 0 ops)]
                  (doseq [op ops]
                    (try 
                      (apply-score-op counter op)
                      (catch Throwable _)))
                  (= model (sut/value counter)))))
