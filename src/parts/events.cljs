(ns parts.events
  (:require [incr.core :as incr]
            [com.rpl.specter :as specter]
            [runtime.widgets :refer [defpart] :as w]))

;;; events

(defpart
  :dom-events
  :part/augment-result
  (fn [ctx params result]
    (update
      result :dom/events merge
      (->> params
           (specter/setval
             [specter/MAP-VALS nil?]
             specter/NONE)))))

(defpart
  :events-handler
  :part/augment-ctx
  (fn [ctx handlers]
    (update ctx ::handlers merge handlers)))

(declare execute-effects!)

(defn dispatch! [event]
  (let [ctx (@w/call-id->ctx (js/parseInt (:event/elem-call-id event)))
        handler (get-in ctx [::handlers (:event event)])]

    (js/console.log "event" event ctx)

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
            ;(js/console.log "FX" fx args)
            (apply h ctx args))
          (js/console.warn "no handler for effect" fx args))))))
