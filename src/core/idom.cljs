(ns core.idom
  (:require [core.incr :as incr]
            [core.engine :as e]
            core.events
            core.styles
            [app.main :as m]
            [cognitect.transit :as t]))


(def transit-w (t/writer :json))
(def transit-r (t/reader :json))


(declare worker)

(defn emit-event! [e]
  ;(js/console.log e)
  (.postMessage worker (t/write transit-w e)))

(defn process-dom-change! [[id x]]
  (js/console.log "Apply diff" id x)

  (when (:tag x)
    ;; todo: probably delete old node with that id if exists
    (let [elem (js/document.createElement (name (:tag x)))]
      (set! (.-id elem) id)
      ;(js/console.log "get " (:parent x) (js/document.getElementById (:parent x)))
      (.appendChild
        (js/document.getElementById (:parent x))
        elem)))
  (when (:text x)
    (let [elem (js/document.getElementById id)]
      (set! (.-innerText elem) (:text x))))

  (when-let [cls (:class x)]
    (let [elem (js/document.getElementById id)]
      (set! (.-className elem) cls)))

  ;; todo, move to worker
  (when-let [s (:styles x)]
    (let [elem (js/document.getElementById id)]
      (set! (.-className elem)
            (core.styles/render-rule elem s))))

  (when-let [v (:value x)]
    (let [elem (js/document.getElementById id)]
      (set! (.-value elem) v)))

  (when-let [parent (:remove-from x)]
    (.removeChild
      (js/document.getElementById parent)
      (js/document.getElementById id)))

  (doseq [[type data] (:events x)
          :when (not= type :core.events/scope-id)]
    (.addEventListener
      (js/document.getElementById id)
      (name type)
      ((fn [data]
         (fn [e]
           (emit-event!
             (assoc data                                    ;:dom-event e
               :core.events/scope-id (:core.events/scope-id (:events x))
               :event/value (.-value (.-target e))
               :event/target-id (.-id (.-target e))
               :event/bounding-rect (.toJSON (.getBoundingClientRect (.-target e))) ))))
        data)))

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
                              :w1)
          fd (e/flat-dom rw)
          obs (incr/diff-thunk (fn []
                                 (emit-dom-diff! @fd)))
          ]
      (incr/add-parent! obs (incr/Observer.))
      (incr/stabilize!)
      ;(js/console.log rw)
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
