(ns core.events
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [com.rpl.specter :as sp]
    [core.computed :as c]))

(def log js/console.log)

(def events-queue (atom []))
(def effects-queue (atom []))

(defn events-scope [ctx handlers]
  (assoc ctx
    ::scope
    {:handlers handlers
     :parent (get ctx ::scope)}))

(def effects-handlers (atom {}))

(defn register-effect-handler! [effect-name handler]
  (swap! effects-handlers assoc effect-name handler))

(declare schedule-recompute!)

(defn dispatch! [ctx event]
  (swap! events-queue conj [ctx event])
  (schedule-recompute!))

(defn process-event
  ([ctx event scope]
   (log "EVENT" event)
   (if-let [f (get-in scope [:handlers (:event event)])]
     (f event ctx)
     (when-let [parent (:parent scope)]
       (process-event ctx event parent))))
  ([ctx event]
    (process-event ctx event (::scope ctx))))

(defn process-events! []
  ;; try
  (doseq [[ctx ev] @events-queue]
    (let [res (process-event ctx ev)]
      (swap! effects-queue conj [ctx res])))
  ;(js/console.log "process-events" @effects-queue)
  (reset! events-queue []))

(defn process-effect! [ctx name data]
  (log "process-effect!" name data)
  ((get-in ctx [::effects name])
    data))

(defn process-effects! []
  ;(js/console.log @effects-queue)
  (c/in-trx
    (fn []
      (doseq [[ctx x] @effects-queue
              [name data] x]
        (process-effect! ctx name data))
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

#_(deftest event-test
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