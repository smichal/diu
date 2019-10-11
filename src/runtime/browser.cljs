(ns runtime.browser
  (:require [cognitect.transit :as t]
            [dom.dom :as dom]
            dom.styles
            dom.events
            ))

(def transit-w (t/writer :json))
(def transit-r (t/reader :json))

(defmulti process-change (fn [elem-ref elem-path attr-path op v]
                           (js/console.log "process-change" elem-ref elem-path attr-path op v)
                           (first attr-path)))
(declare worker)


(defmethod process-change :events [elem-ref elem-path attr-path op data]
  (when (#{:+ :r} op)
    (let [[_ type & data-path] attr-path]
      (cond
        (nil? type) (add-events elem-ref data (= op :r))
        (nil? data-path) (add-event elem-ref type data (= op :r))
        :else (change-event-data elem-ref type data-path data))))
  (when (= :- op)
    (let [[_ type & data-path] attr-path]
      (cond
        (nil? type) (remove-events elem-ref)
        (nil? data-path) (remove-event elem-ref type)
        :else nil ;; todo
        ))))


(defn process-dom-change! [[path op v]]
  (dom/apply-change {:node-path []} (concat [:dom/children :root] path) op v))



(defn init-dom-mutator! [worker]
  (let [elem (js/document.createElement "div")]
    (set! (.-id elem) "root")
    (js/document.body.appendChild elem))

  (dom.styles/insert-styles!)

  (set! (.-onmessage worker)
        (fn [e]
          ;(js/console.log (t/read transit-r (.-data e)))
          ;(js/console.profile "render")
          (mapv process-dom-change! (t/read transit-r (.-data e)))
          ;(js/console.profileEnd "render")
          )))

(defn start-worker! []
  ; SharedWorker.
  (let [worker (js/Worker. "js/worker.js")]
    ;(.start (.-port worker))
    (js/console.log "worker" worker)
    (def worker worker)
    (aset js/window "__worker" worker)
    (init-dom-mutator! worker)))

(defn init []
  (println "init")
  (start-worker!)
  )