(ns parts.vaadin-parts
  (:require [incr.core :as incr]
            [com.rpl.specter :as specter]
            [runtime.widgets :refer [defpart] :as w]))


(defpart
  :vaadin-dialog
  :part/render
  (fn [ctx params]
    (cond->
      {:dom/tag "vaadin-dialog"
       :dom/call-id (:runtime.widgets/call-id ctx) ;; only for editor
       :vaadin/dialog {:opened (:opened params)
                       :content (vec (map-indexed
                                   (fn [i x]
                                     (incr/incr w/call (dissoc ctx :params) x i))
                                   (incr/value (:children params))))}}
      (:opened-changed params) (assoc :dom/events {:opened-changed (:opened-changed params)})
      )))