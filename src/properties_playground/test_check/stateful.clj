(ns properties-playground.test-check.stateful)

(defn new-counter []
  (atom 0))

(defn add [counter val]
  (assert (not (neg? val)))
  (swap! counter + val)
  nil)

(defn value [counter]
  @counter)

