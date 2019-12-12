(ns parts.params
  (:require [cljs.spec.alpha :as s]
            [malli.core :as m]
            ))



(s/def ::string string?)
(s/def ::tag-name (s/or :s string? :k keyword?))

(s/def ::dom-attrs (s/map-of string? string?))

(s/def ::child (constantly true))
(s/def ::children (s/coll-of ::child))

(s/def ::widget keyword?)
(s/def ::params (s/map-of #(or string? keyword?) (constantly true))) ;;fixme

(s/def ::locals (s/map-of keyword? (constantly true)))


(def type-defs
  (atom {}))

(defn type-def [k]
  (get @type-defs k))

(def type-registry
  (atom m/default-registry))

(defn register! [k schema]
  (swap! type-registry assoc k (m/schema schema {:registry @type-registry}))
  (swap! type-defs assoc k schema)
  k)

(register! ::child any?)
(register! ::children [:vector ::child])
(register! ::params [:map-of keyword? any?])
(register! ::widget-call [:map
                          [:widget keyword?]
                          [:params ::params]])
(register! ::locals [:map-of keyword? any?])

(defn form [s]
  (m/form s {:registry @type-registry}))
