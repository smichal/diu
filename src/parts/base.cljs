(ns parts.base
  (:require [incr.core :as incr])
  (:use [runtime.widgets :only [defpart resolve-widget call]]))

(defpart
  :dom
  :part/render
  (fn [ctx params]
    (cond->
      {:dom/tag (:tag params)
       :dom/call-id (:runtime.widgets/call-id ctx) ;; only for editor
       }
      (:text params) (assoc :dom/text (:text params))
      (:children params) (assoc :dom/children (mapv
                                                (fn [x]
                                                  (incr/incr call ctx x))
                                                (incr/value (:children params))))
      (:attrs params) (assoc :dom/attrs (:attrs params))
      )))

(defpart
  :widget
  :part/render
  (fn [ctx {:keys [widget params]}]
    ;; todo: incr?
    (incr/incr
      call
      (assoc ctx :params params)
      (resolve-widget ctx widget)
      )))

(defpart
  :local-state
  :part/augment-ctx
  (fn [ctx params]
    (update ctx :scope merge (map
                               (fn [[k v]]
                                 [k (if (implements? IDeref v) v (incr/cell v))])
                               params))))

(defpart
  :locals
  :part/augment-ctx
  (fn [ctx params]
    (update ctx :scope merge params)))


;; now for maps, not vectors
(defpart
  :list-of
  :part/render (fn [ctx {:keys [items
                                item-widget
                                param-for-key
                                param-for-value
                                common-params]}]
                 {:dom/tag :div
                  :dom/children (mapv                       ;; todo: incr map
                                  (fn [[k v]]
                                    (incr/incr call
                                               (assoc ctx :params
                                                          (assoc (or common-params {})
                                                            param-for-key k
                                                             param-for-value v))
                                               (resolve-widget ctx item-widget)
                                               ))
                                  (incr/value items))}
                 ))
