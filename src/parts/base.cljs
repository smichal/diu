(ns parts.base
  (:require [incr.core :as incr])
  (:use [runtime.widgets :only [defpart resolve-widget call]]))

(defpart
  :dom
  :part/render
  (fn [ctx params]
    (if (:text params)
      {:dom/tag (:tag params)
       :dom/text (:text params)}
      {:dom/tag (:tag params)
       :dom/children (mapv
                       #(incr/incr call ctx %)
                       (incr/value (:children params)))})))

(defpart
  :widget
  :part/render
  (fn [ctx {:keys [widget params]}]
    (call (assoc ctx :params params)
          (resolve-widget ctx widget))))



#_(defpart
    :list-of
    :part/render (fn [ctx {:keys [widget params]}]))
