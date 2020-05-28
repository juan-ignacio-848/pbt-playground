(ns properties-playground.test-check.stateful)

(defn new-counter []
  (atom 0))

(defn add [counter val]
  (assert (not (neg? val)))
  (swap! counter + val)
  nil)

(defn value [counter]
  @counter)

(defn new-kvs []
  (atom {}))

(defn kv-clr [kvs]
  (reset! kvs {})
  nil)

(defn kv-put [kvs k v]
  (swap! kvs assoc k v)
  nil)

(defn kv-get [kvs k]
  (get @kvs k))

(defn kv-del [kvs k]
  (swap! kvs dissoc k)
  nil)
