(ns runtime.browser
  (:require [cognitect.transit :as t]
            [dom.dom :as dom]
            dom.styles
            dom.events
            dom.docker-layout
            dom.vaadin-elements
            ))

(def transit-w (t/writer :json))
(def transit-r (t/reader :json))

(defmulti process-change (fn [elem-ref elem-path attr-path op v]
                           (js/console.log "process-change" elem-ref elem-path attr-path op v)
                           (first attr-path)))
(declare worker)

(defn process-dom-change! [[path op v]]
  (dom/apply-change {:node-path []} (concat [:dom/children :root] path) op v))

(defn init-dom-mutator! [worker]
  (let [elem (js/document.createElement "div")]
    (set! (.-id elem) "root")
    (js/document.body.appendChild elem))

  (dom.styles/insert-styles!)

  (set! (.-onmessage worker)
        (fn [e]
          (js/console.log "diff" (t/read transit-r (.-data e)))
          ;(js/console.profile "render")
          (doseq [x (t/read transit-r (.-data e))]
            (process-dom-change! x))
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

(defn ^:export init []
  (println "init")
  (start-worker!)
  )