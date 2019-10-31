(ns parts.events
  (:require [incr.core :as incr]
            [com.rpl.specter :as specter]
            [runtime.widgets :refer [defpart] :as w]))

;;; events

(defpart
  :dom-events
  :part/name "Trigger events"
  :part/desc "Attaches events to the widget"
  :part/augment-result
  (fn [ctx params result]
    (update
      result :dom/events merge
      (->> params
           (specter/setval
             [specter/MAP-VALS nil?]
             specter/NONE)
           incr/deep-deref
           ))))

(defpart
  :events-handler
  :part/name "Event handlers"
  :part/desc "Sets handlers for events"
  :part/augment-ctx
  (fn [ctx handlers]
    (update ctx ::handlers merge handlers)))

(declare execute-effects!)

(defn dispatch! [event]
  (let [ctx (@w/call-id->ctx
              (-> (:event/elem-call-id event)
                  (clojure.string/split " ")
                  last
                  js/parseInt))
        handler (get-in ctx [::handlers (:event event)])]

    (js/console.log "event" event #_ctx)

    (if handler
      (execute-effects! (handler event ctx) ctx)
      (js/console.warn "event without handler" event))))


(def effects-handlers (atom {}))

(defn register-effect-handler! [name handler]
  (when-let [h (@effects-handlers name)]
    (when (not= h handler)
      (js/console.warn "handler for" name "already exists")))
  (swap! effects-handlers assoc name handler))

;; as map?
(defn execute-effects! [fxs ctx]
  (when fxs
    (let [fxs (if (vector? (first fxs)) fxs [fxs])]
      (doseq [[fx & args] fxs]
        ; try
        (if-let [h (@effects-handlers fx)]
          (do
            (js/console.log "FX" fx args)
            (apply h ctx args))
          (js/console.warn "no handler for effect" fx args))))
    (incr/schedule-stabilization!)
    ))

(register-effect-handler!
  :emit-event
  (fn [_ event]
    (dispatch! event)))

(register-effect-handler!
  :delay
  (fn [ctx time effects]
    (js/console.log :delay time effects)
    (js/setTimeout
      (fn []
        (execute-effects! effects ctx))
      time)))

(defpart
  :bind-input
  :part/name "Bind input"
  :part/augment-result
  (fn [ctx params result]
    (assoc-in result [:dom/events :input]
              {:event ::bind-input}))
  :part/augment-ctx
  (fn [ctx params]
    (assoc-in ctx [::handlers ::bind-input]
              (fn [event ctx]
                [[:set-local (:to params) (:event/value event)]]))))