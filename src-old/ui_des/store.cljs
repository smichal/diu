(ns ui-des.store
  (:require [hasch.core :as hasch]
            [smidjen.core :refer-macros [facts fact]]
            [ui-des.computed :as com]))


(def ^:dynamic *store* (atom {}))

(defprotocol IEntity
  (eid [this]))

(extend-type js/Function
  hasch.benc/PHashCoercion
  (-coerce [this md-create-fn write-handlers]
    (hasch.platform/encode
     (byte 100)
     (hasch.benc/encode-safe (hasch.platform/str->utf8 (str this)) md-create-fn))))

(extend-type PersistentArrayMap
  IEntity
  (eid [this] (hasch/uuid this)))

(extend-type PersistentVector
  IEntity
  (eid [this] (hasch/uuid this)))

(extend-type UUID
  IEntity
  (eid [this] this)
  ILookup
  (-lookup [this key]
    (com/computed
     (get @this key))
    #_(sig/block
       (fn []
         (get-in @*store* [this key]))
       true    ; impure block, fixme
))
  IDeref
  (-deref [this]
    #_(sig/return-signal-value this (get @*store* this))
    #_(com/observable
       (get @*store* this)
       (fn [])
       (fn []))
    (get @*store* this))
  com/IObservable
  (-value [this] @this)

  ;; delegate to value signal if possible, maybe upper methods too
  (add-subscriber! [this s])
  (remove-subscriber! [this s]))


#_(defprotocol ITransactable
    (transact [this trx]))

(defn add-entity [ent]
  (let [id (eid ent)]
    (swap! *store* assoc id ent)
    id))

(def ent add-entity)

;; queue list of trx, each trx can contain multiple ops
;; ops-map in metadata of entity

;; notify changes to listening blocks
(defn transact! [eid trx]
  (ui-des.dom/schedule-update!)
  (let [new-val (trx @eid)]
    (swap! *store* assoc eid new-val)))

(facts
 "Store"
 (let [a (add-entity {:hello "world"})
       b (add-entity {:list [a]})]
   (get-in @*store* [b :list 0 :hello]) => "world"))

#_(facts
 "store signals"
 (let [a (add-entity {:a "text"
                      :b 42})
       x (sig/block (fn []
                      (str (get a :a) " 123")))
       comp-counter (atom 0)
       y (sig/block (fn []
                      (swap! comp-counter inc)
                      (inc (get a :b))))]
   @x => "text 123"
   @comp-counter => 0
   @y => 43
   @comp-counter => 1
   (transact! a #(assoc % :a "asd"))
   @x => "asd 123"
   @comp-counter => 1))