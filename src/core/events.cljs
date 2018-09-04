(ns core.events
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [com.rpl.specter :as sp]
    [core.computed :as c]))


(def events-queue (atom []))
(def effects-queue (atom []))

(defn new-event-context [parent]
  (atom
    {:handlers {}
     :parent parent}))


(defn events-scope [ctx handlers]
  (assoc ctx
    ::scope
    {:handlers handlers
     :parent (get ctx ::scope)}))

(def effects-handlers (atom {}))

(defn register-event-handler! [ctx event-name handler]
  (swap! ctx
         #(sp/setval [:handlers event-name sp/END] [handler] %)))

(defn register-effect-handler! [effect-name handler]
  (swap! effects-handlers assoc effect-name handler))

(declare schedule-recompute!)

(defn dispatch! [ctx event]
  (swap! events-queue conj [ctx event])
  (schedule-recompute!))


(defn process-event [ctx event]
  ;; fix ctx
  ;(js/console.log ctx event)
  (for [f (get-in @ctx [:handlers (:event event)])]
    (f @ctx event)))

(defn process-events! []
  ;; try
  (doseq [ev @events-queue]
    (let [res (apply process-event ev)]
      (swap! effects-queue concat res)))
  ;(js/console.log "process-events" @effects-queue)
  (reset! events-queue []))

(defn process-effect! [name data]
  ;(js/console.log "process-effect!" name)
  ((@effects-handlers name)
    data))

(defn process-effects! []
  ;(js/console.log @effects-queue)
  (c/in-trx
    (fn []
      (doseq [x @effects-queue
              [name data] x]
        (process-effect! name data))
      (reset! effects-queue []))))



(def processing-scheduled? (atom false))

(defn recompute! []
  (process-events!)
  (process-effects!)
  (reset! processing-scheduled? false))

(defn schedule-recompute! []
  (when-not @processing-scheduled?
    (reset! processing-scheduled? true)
    (js/requestAnimationFrame recompute!)))

(deftest event-test
  (reset! events-queue [])
  (reset! effects-queue [])
  (let [c (new-event-context nil)
        world (atom nil)]
    (register-event-handler! c :test (fn [_ x] {:fx x}))
    (register-effect-handler! :fx #(reset! world %))

    (dispatch! c {:event :test})
    (is (= @events-queue [[c {:event :test}]]))

    (process-events!)
    (is (= @events-queue []))
    (is (= @effects-queue [{:fx {:event :test}}]))

    (process-effects!)
    (is (= @world {:event :test})))
  )