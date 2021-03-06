(ns parts.base
  (:require [incr.core :as incr]
            [parts.params :as params])
  (:use [runtime.widgets :only [defpart resolve-widget call]]))

(defpart
  :dom
  :part/name "HTML Element"
  :part/desc "Creates DOM element with given tag, attributes, and content"
  :part/params
  [::params/params
   [:tag
    {:param/name "Tag"
     :param/default :div}
    string?
    ;[:enum :div :p :h1]
    ]
   [:attrs
    {:param/name "Attributes"}
    [:map-of string? string?]]
   [:text
    {:param/name "Text"
     :param/default "Text..."}
    string?]
   [:children
    {:param/name "Children"
     :param/default []}
    ::params/children]]

  :part/render
  (fn [ctx params]
    ;(js/console.log params) ;todo check deep-deref here
    (cond->
      {:dom/tag (or (:tag params) :div)
       :dom/call-id (:runtime.widgets/call-id ctx) ;; only for editor
       }
      (:text params) (assoc :dom/text (:text params))
      (:children params) (assoc :dom/children (vec
                                                (map-indexed
                                                  (fn [i x]
                                                    (incr/incr call (dissoc ctx :params) x i))
                                                  (incr/value (:children params)))))
      (:attrs params) (assoc :dom/attrs (:attrs params))
      )))

(defpart
  :widget
  :part/name "Call widget"
  :part/desc "Calls other widget with given params"
  :part/params ::params/widget-call
  :part/render
  (fn [ctx {:keys [widget params]}]
    ;; todo: incr?
    (->
      @(incr/incr
        call
        (assoc ctx :params params)
        (resolve-widget ctx widget)
        )
      (update :dom/call-id #(str (:runtime.widgets/call-id ctx) " " %))
      )))

(defn add-local-state [ctx params]
  (update ctx :scope merge (map
                             (fn [[k v]]
                               [k (if (implements? IDeref v) v (incr/cell v))])
                             params)))

(defpart
  :local-state
  :part/name "Local state"
  :part/desc "Adds local state to widget"
  :part/params ::params/locals
  :part/augment-ctx
  (fn [ctx params]
    @(incr/incr add-local-state ctx params)))

(defpart
  :locals
  :part/name "Locals"
  :part/desc "Sets constant values in widget's scope"
  :part/params ::params/locals
  :part/augment-ctx
  (fn [ctx params]
    (update ctx :scope merge params)))


;; now for maps, not vectors
(defpart
  :list-of
  :part/name "List of"
  :part/desc "Creates widget for each element of list"
  :part/params
  [::params/params
   [:items any?]
   [:item-widget keyword?    ;::params/child
    ]
   [:param-for-key keyword?]
   [:param-for-value keyword?]
   [:common-params [:map-of keyword? any?]]
   ]
  :part/render (fn [ctx {:keys [items
                                item-widget
                                param-for-key
                                param-for-value
                                common-params]}]
                 {:dom/tag :div
                  :dom/call-id (:runtime.widgets/call-id ctx)
                  :dom/children
                  (let [items (incr/value items)
                        items (if (map? items)
                               items
                               (map vector (range) items)
                               )]
                    ;(js/console.log "LIST" items)
                    (mapv                                   ;; todo: incr map
                      (fn [[k v]]
                        (incr/incr call
                                   (assoc ctx :params
                                              (assoc (or common-params {})
                                                (or param-for-key :key) k
                                                (or param-for-value :item) v))
                                   (resolve-widget ctx item-widget)
                                   k))
                      items))}
                 ))

(defn if-widget [ctx cond then else]
  (if (incr/value cond)
    (call ctx then :t)
    (if else
      (call ctx else :e)
      {:dom/tag :span
       :dom/call-id (:runtime.widgets/call-id ctx)
       }
      )))

(defpart
  :if
  :part/name "If"
  :part/desc "Conditional choice between widgets"
  :part/render (fn [ctx {:keys [cond then else]}]
                 (incr/incr if-widget ctx cond then else)))

(defpart
  :meta
  :part/name "Metadata"
  :part/params
  [::params/params
   [:part/params {:param/name "Params"} ::params/params]]

  )