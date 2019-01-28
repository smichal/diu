(ns core.idom
  (:require [core.incr :as incr]
            [core.engine :as e]
            [cognitect.transit :as t]))


(def test-dom
  {:tag :div
   :children
   {1 {:tag :p
       :text "first"}
    2 {:tag :p
       :text "second"}}})


(defn process-dom-change! [[id x]]
  ;(js/console.log "Apply diff" id x)

  (when (:tag x)
    (let [elem (js/document.createElement (name (:tag x)))]
      (set! (.-id elem) id)
      ;(js/console.log "get " (:parent x) (js/document.getElementById (:parent x)))
      (.appendChild
        (js/document.getElementById (:parent x))
        elem)))
  (when (:text x)
    (let [elem (js/document.getElementById id)]
      (set! (.-innerText elem) (:text x))))

  (when-let [parent (:remove-from x)]
    (.removeChild
      (js/document.getElementById parent)
      (js/document.getElementById id)))

  )

(defn init-dom-mutator! [worker]
  (let [elem (js/document.createElement "div")]
    (set! (.-id elem) "root")
    (js/document.body.appendChild elem))

  (set! (.-onclick js/document.body)
        (fn []
          (.postMessage worker 1)
          ))

  (let [r (t/reader :json)]
    (set! (.-onmessage worker)
          (fn [e]
            (js/console.profile "render")
            (mapv process-dom-change! (t/read r (.-data e)))
            (js/console.profileEnd "render")
            )))
  )

(defn start-worker []
  (let [worker (js/Worker. "js/worker.js")]
    ;(.postMessage worker "m->w")
    (init-dom-mutator! worker)
    #_(set! (.-onmessage worker)
          (fn [e]
            (js/console.log "w->m" (.-data e))
            ))
    )



  )


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
  (set! js/onmessage (fn [e]
                       ;(js/console.log "worker" e)
                       (js/postMessage "response")
                       ))
  (def w (t/writer :json))

  (defn emit-dom-diff! [diff]
    ;(js/console.log "emit diff" diff)
    (js/postMessage (t/write transit-w diff))
    )


  (defn tt []
    (let [rw (e/render-widget e/ctx-0 :w1)
          fd (e/flat-dom rw)
          obs (incr/diff-thunk (fn []
                                 (emit-dom-diff! @fd)))
          ]
      (incr/add-parent! obs (incr/Observer.))
      (incr/stabilize!)
      (js/console.log rw)
      obs))
  (def _a (tt))

  (set! js/onmessage
        (fn []
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
