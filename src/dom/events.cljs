(ns dom.events
  (:require [dom.dom :as dom :refer [add-diff-reducer]]
            [clojure.core.match :refer [match]]
            [com.rpl.specter :as specter]
            [cognitect.transit :as t]))

(defonce events-per-elem (js/WeakMap.))

(def transit-w (t/writer :json))

(defn emit-event! [e]
  (.postMessage (aget js/window "__worker")
                (t/write transit-w e)))

(defn add-event [elem type data replace?]
  (let [node-events (.get events-per-elem elem)
        node-events (assoc node-events type data)]
    (.set events-per-elem elem node-events)
    (when-not replace?
      (.addEventListener
        elem
        (name type)
        dom-event-handler))))

(defn change-event-data [elem type path data]
  (.set events-per-elem elem
        (update
          (.get events-per-elem elem)
          type
          #(update-in % path data))))

(defn add-events [elem events replace?]
  (doseq [[type data] events]
    (add-event elem type data replace?)))

(defn remove-event [elem type]
  (let [node-events (.get events-per-elem elem)
        node-events (dissoc node-events type)]
    (.set events-per-elem elem node-events)
    (.removeEventListener
      elem
      (name type)
      dom-event-handler)))

(defn remove-events [elem]
  (let [node-events (.get events-per-elem elem)]
    (doseq [[type _] node-events]
      (remove-event elem type))))

(defn dom-event-handler [e]
  (let [data (get
               (.get events-per-elem (.-currentTarget e))
               (keyword (.-type e)))]
    (emit-event!
      (assoc data                                  ;:dom-event e
        :event/value (.-value (.-target e))
        :event/target-id (.-id (.-target e))
        :event/bounding-rect (.toJSON (.getBoundingClientRect (.-target e)))
        :event/key-pressed (.-key e)
        ))))

(add-diff-reducer
  :dom/events
  (fn [{:keys [node-path]} path op data]
    (let [elem (dom/get-node node-path)]
     (match [path op]
            [[type] :+] (add-event elem type data false)
            [[type] :r] (add-event elem type data true)
            [[type] :-] (remove-event elem type)
            [[type & r] _] (change-event-data elem type r data)
            [[] :+] (add-events elem data false)
            [[] :r] (add-events elem data true)
            [[] :-] (remove-events elem)))))
