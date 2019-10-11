(ns parts.events
  (:require [incr.core :as incr]
            [com.rpl.specter :as specter])
  (:use [runtime.widgets :only [defpart]]))

;;; events

(defpart
  :dom-events
  :part/augment-result
  (fn [ctx params result]
    (update
      result :dom/events merge
      (specter/transform
        [specter/MAP-VALS]
        #(assoc % ::scope-id (::scope-id ctx))
        params))))

(def event-scopes (atom {}))

(defpart
  :events-handler
  :part/augment-ctx
  (fn [ctx handlers]
    (let [sid (hash handlers)
          s {:handlers handlers
             :ctx ctx
             :parent (get ctx ::scope-id)}]
      (swap! event-scopes assoc sid s)
      (assoc ctx ::scope-id sid))))


(declare execute-effects!)

(defn dispatch! [event]
  (println "event" event)
  (loop [scope-id (::scope-id event)]
    (let [scope (@event-scopes scope-id)
          ctx (:ctx scope)
          handler (get-in scope [:handlers (:event event)])]
      ;(println scope)
      (if handler
        (execute-effects! (handler event ctx) ctx)
        (if-let [parent (:parent scope)]
          (recur parent)
          (js/console.warn "event without handler" event))))))


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
          (js/console.warn "no handler for effect" fx))))))
