(ns core.events
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [mount.core :refer [defstate]]))


#_(defstate events-queue
  :start (fn []))

(def events-queue (atom []))
(def effects-queue (atom []))

(def log js/console.log)

#_(defn ctx-dispatch [handlers event]
  (log "EVENT" event)
  (let [f (get handlers (:event event))]
    (f event ctx)))

(def last-scope-id (atom 0))
(def scopes (atom {}))
(defn next-scope-id! []
  (swap! last-scope-id inc))
;; todo: GC

(defn add-handlers [ctx handlers]
  (let [sid (hash handlers)
        s {:handlers handlers
           :ctx ctx
           :parent (get ctx ::scope-id)}]
    (swap! scopes assoc sid s)
    ;(println "EV SCOPE" sid s)
    (assoc ctx ::scope-id sid
               ;::dispatch (partial ctx-dispatch handlers)
               )))

(def app-cfg
  {:parts
   {:events-handler {:intercept {:before add-handlers}}
    :dom-events {:intercept {:after (fn [ctx params]
                                      (assoc-in ctx [:dom :events] (assoc params ::scope-id (::scope-id ctx)))
                                      )}}
    }})

(defn dispatch! [event]
  ;(println "event" event)
  (loop [scope-id (::scope-id event)]
    (let [scope (@scopes scope-id)
          ctx (:ctx scope)
          handler (get-in scope [:handlers (:event event)])]
      (if handler
        (handler event ctx)
        (when-let [parent (:parent scope)]
          (recur parent))))))


