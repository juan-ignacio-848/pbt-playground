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

;; Key value storage
;; Operations: kv-put, kv-del, kv-get, kv-clr
(defn gen-put [gen-key gen-val]
  (gen/tuple (gen/return :put) gen-key gen-val))

(defn gen-get [gen-key]
  (gen/tuple (gen/return :get) gen-key))

(defn gen-del [gen-key]
  (gen/tuple (gen/return :del) gen-key))

(def gen-clr (gen/tuple (gen/return :clr)))

(defn gen-kv-op [gen-key gen-val]
  (gen/one-of [(gen-put gen-key gen-val)
               (gen-get gen-key)
               (gen-del gen-key)
               gen-clr]))

(defn apply-kvs-model-op [model [op k v]]
  (case op
    :clr {:not-deleted {}
          :deleted     #{}}
    :put {:not-deleted (assoc (:not-deleted model) k v)
          :deleted     (disj (:deleted model #{}) k)}
    :get (get (:not-deleted model) k)
    :del {:not-delted (dissoc (:not-deleted model) k)
          :deleted    (conj (:deleted model #{}) k)}
    ))

(defn apply-kvs-op [kvs [op k v]]
  (case op
    :clr (sut/kv-clr kvs)
    :put (sut/kv-put kvs k v)
    :get (sut/kv-get kvs k)
    :del (sut/kv-del kvs k)))

(defspec kvs-model-test
  (prop/for-all [ops (gen/vector (gen-kv-op gen/keyword gen/string))]
                (let [model (reduce apply-kvs-model-op {} ops)
                      kvs   (sut/new-kvs)]
                  (doseq [op ops]
                    (apply-kvs-op kvs op))
                  (every? (fn [[k v]]
                            (= (sut/kv-get kvs k) v))
                          (:not-deleted model))
                  (every? (fn [k]
                            (nil? (sut/kv-get kvs k)))
                          (:deleted model)))))

(comment
  (reduce apply-kvs-model-op {} [[:put :a 3] [:put :b 3] [:put :a 8] [:del :b] [:clr]])
  (apply-kvs-model-op {} [:clr])
  (apply-kvs-model-op {} [:put :a 3])
  (apply-kvs-model-op {:a 5} [:get :a])
  (apply-kvs-model-op {:a 2 :b 3} [:del :a])
  (gen/sample (gen-kv-op gen/nat gen/string))
  (gen/sample (gen-get gen/nat))
  (gen/sample (gen-del gen/nat))
  (gen/sample gen-clr)
  (gen/sample (gen-put gen/nat gen/string))
  )
