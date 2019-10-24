(ns parts.params
  (:require [cljs.spec.alpha :as s]
            [spec-tools.data-spec :as ds]))



(s/def ::string string?)
(s/def ::tag-name (s/or :s string? :k keyword?))

(s/def ::dom-attrs (s/map-of string? string?))

(s/def ::child (constantly true))
(s/def ::children (s/coll-of ::child))
