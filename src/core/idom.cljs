(ns core.idom
  (:require [core.incr :as incr]
            [core.engine :as e]
            core.events
            core.styles
            [app.main :as m]
            [cognitect.transit :as t]
            clojure.string))


(def transit-w (t/writer :json))
(def transit-r (t/reader :json))


(declare worker)

(defn emit-event! [e]
  ;(js/console.log e)
  (.postMessage worker (t/write transit-w e)))

(def EVENTS "abort abort abort afterprint animationend animationiteration animationstart audioprocess beforeprint beforeunload beginEvent blocked blur cached canplay canplaythrough change chargingchange chargingtimechange checking click close compassneedscalibration complete compositionend compositionstart compositionupdate contextmenu copy cut dblclick devicelight devicemotion deviceorientation deviceproximity dischargingtimechange downloading drag dragend dragenter dragleave dragover dragstart drop durationchange emptied ended ended endEvent error focus focusin focusout fullscreenchange fullscreenerror gamepadconnected gamepaddisconnected hashchange input invalid keydown keypress keyup languagechange The definition of 'NavigatorLanguage.languages' in that specification. levelchange load loadeddata loadedmetadata loadend loadstart message mousedown mouseenter mouseleave mousemove mouseout mouseover mouseup noupdate obsolete offline online open open orientationchange pagehide pageshow paste pause pointerlockchange pointerlockerror play playing popstate progress progress ratechange readystatechange repeatEvent reset resize scroll seeked seeking select show stalled storage submit success suspend SVGAbort SVGError SVGLoad SVGResize SVGScroll SVGUnload SVGZoom timeout timeupdate touchcancel touchend touchenter touchleave touchmove touchstart transitionend unload updateready upgradeneeded userproximity versionchange visibilitychange volumechange waiting wheel")
(defonce events-per-elem (zipmap (clojure.string/split EVENTS #" ")
                                 (repeatedly #(js/WeakMap.))))

(defn dom-event-handler [e]
  (let [data (.get (events-per-elem (.-type e)) (.-currentTarget e))]
    (emit-event!
      (assoc data                                  ;:dom-event e
        :event/value (.-value (.-target e))
        :event/target-id (.-id (.-target e))
        :event/bounding-rect (.toJSON (.getBoundingClientRect (.-target e)))
        :event/key-pressed (.-key e)
        ))))

(defn set-events! [elem events]
  (doseq [[type data] events
          :when (not= type :core.events/scope-id)]
    (let [memo (get events-per-elem (name type))
          data (if-let [sid (:core.events/scope-id events)]
                 (assoc data :core.events/scope-id sid)
                 data)
          data (merge (.get memo elem) data)]
      (.set memo elem data)
      (.addEventListener
        elem
        (name type)
        dom-event-handler))))

(defn maybe [x else]
  (if (not= x ::incr/deleted) x else))

(defn process-dom-change! [[id x]]
  ;(js/console.log "Apply diff" id x)

  (when (:tag x)
    ;; todo: probably delete old node with that id if exists
    (when-let [elem (js/document.getElementById id)]
      (.remove elem))

    ;; todo: optimize, index of children/ids instead of scan
    (let [elem (js/document.createElement (name (:tag x)))]
      (set! (.-id elem) id)
      ;(js/console.log "get " (:parent x) (js/document.getElementById (:parent x)))
      (let [parent (js/document.getElementById (:parent x))
            children (js/Array.from (.-children parent))
            before-node (first (filter #(< id (.-id %)) children))]
        (.insertBefore parent elem before-node))

      #_(.appendChild
          (js/document.getElementById (:parent x))
          elem)))

  (when (:text x)
    (let [elem (js/document.getElementById id)]
      (set! (.-innerText elem) (maybe (:text x) nil))))

  (when-let [cls (:class x)]
    (let [elem (js/document.getElementById id)]
      (set! (.-className elem) (maybe cls nil))))

  ;; todo, move to worker
  (when-let [s (maybe (:styles x) {})]
    (let [elem (js/document.getElementById id)]
      (set! (.-className elem)
            (core.styles/render-rule elem s))))

  (when-let [v (:value x)]
    (let [elem (js/document.getElementById id)]
      (set! (.-value elem) (maybe v nil))))

  (when-let [parent (:remove-from x)]
    (.remove (js/document.getElementById id))
    #_(.removeChild
      (js/document.getElementById parent)
      (js/document.getElementById id)))

  (when-let [events (:events x)]
    (set-events! (js/document.getElementById id) (maybe events {})))

  (when-let [attrs (maybe (:attrs x) {})]
    (let [elem (js/document.getElementById id)]
      ;; handle diffs
      (doseq [[attr val] attrs]
        (aset elem (name attr) val))

      (when (:autofocus attrs)
        (.focus elem))

      ))

  )


(defn init-dom-mutator! [worker]
  (let [elem (js/document.createElement "div")]
    (set! (.-id elem) "root")
    (js/document.body.appendChild elem))

  (set! (.-onmessage worker)
        (fn [e]
          ;(js/console.profile "render")
          (mapv process-dom-change! (t/read transit-r (.-data e)))
          ;(js/console.profileEnd "render")
          ))

  (def worker worker)
  #_(set! (.-onclick js/document.body)
        (fn []
          (.postMessage worker 1)
          ))

  (core.styles/init-dom-renderer!)

  )

(defn start-worker []
  (let [worker (js/Worker. "js/worker.js")]
    (init-dom-mutator! worker)
    ))


(defn ^:dev/after-load start []
  (js/console.log "start")
  )

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")
  (start)

  (start-worker)
  )

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))

(when-not (exists? js/window)

  (defn emit-dom-diff! [diff]
    ;(js/console.log "emit diff" diff)
    (js/postMessage (t/write transit-w diff)))


  (defn tt []
    (let [rw (e/render-widget (assoc m/ctx-0
                                :widget/path [:w1])
                              :w1
                              nil)
          fd (e/flat-dom rw)
          obs (incr/diff-thunk (fn []
                                 ;(println "OBS")
                                 (emit-dom-diff! @fd)))
          ]
      (incr/add-parent! obs (incr/Observer.))
      (incr/stabilize!)
      ;(js/console.log fd)
      (set! (.-fd js/self) fd)
      (set! (.-obs js/self) obs)

      obs))
  (def _a (tt))

  (set! js/onmessage
        (fn [e]
          (core.events/dispatch!
            (t/read transit-r (.-data e)))
          (incr/schedule-stabilization!)
          )
        #_(fn []
          (incr/cell-swap! e/state-cell #(assoc-in % [:widgets :w :w-p :text] "123"))
          (incr/cell-swap! e/state-cell #(update-in % [:widgets :w1 :container :children] dissoc 0))
          (js/console.profile "stabilize")
          (incr/stabilize!)
          (js/console.profileEnd "stabilize")
          ))

  #_(let [c (incr/diff-cell test-dom)
        node (fn node [parent prefix x f]
               (into {}
                     (cons
                       (when-let [nd (f parent x)] [prefix nd])
                       (map (fn [[k v]]
                              (node prefix (str prefix "-" k) v f))
                            (:children x)))))
        flat-dom (incr/diff-thunk
                   (fn []
                     (node "root" "N" @c
                           (fn [p x]
                             (merge
                               (if (string? (:text x)) {:text (:text x)})
                               (if (:tag x) {:tag (:tag x)})
                               {:parent p}
                               )))))

        emit-diffs (incr/diff-thunk
                     (fn []
                       (emit-dom-diff! @flat-dom)
                       ))
        ]
        (incr/add-parent! emit-diffs (incr/Observer.))
        (incr/stabilize!)

        (set! js/onmessage
              (fn []
                (incr/path-diff-cell! c {:children {(rand-int 100) {:tag :p :text "third"}}})
                ;(js/console.profile "stabilize")
                (incr/stabilize!)
                ;(js/console.profileEnd "stabilize")
                ))

        #_(js/setTimeout (fn []
                         (incr/path-diff-cell! c {:children {3 {:tag :p :text "third"}}})
                         (incr/stabilize!))
                       3000)

        )



  )
