(ns runtime.worker
  (:require
    [cognitect.transit :as t]
    [runtime.widgets :as w]
    [incr.core :as incr :include-macros true]
    [editscript.core :as ec]
    [editscript.edit :as ee]
    parts.base
    parts.events
    parts.styles
    ))

(println "worker started")

(def transit-w (t/writer :json))
(def transit-r (t/reader :json))

(defn post-message [m]
  (js/postMessage
    (t/write transit-w m)))

(def prev-value (atom nil))

(defn emit-changes [v]
  (let [diffs (ee/get-edits (ec/diff @prev-value v))]
    (reset! prev-value v)
    (post-message diffs)))

(defn emit-widget [ctx w]
  (let [r (incr/incr w/call ctx w)]
    ;(js/console.log "WWW" @r)
    (emit-changes @r)
    nil
    #_(incr/stabilize!)))

(def c (incr/cell {:a 1 :b 2 :c 1}))

(parts.events/register-effect-handler!
  :test
  (fn [ctx d]
    (println "TT" ctx d)
    (reset! c {:a (rand-int 100) :b 4 :c 1})
    )
  )

(def app
  (incr/cell {:events-handler {
                               :button-clicked (fn [e ctx]
                                                 (println e ctx)
                                                 [[:test {:test 1}]])
                               }
              :dom {:tag :div
                    :children
                    [{:set-styles {:color (incr/thunk (if (= 1 @(incr/incr get c :c)) "red" "green"))}
                      :dom {:tag :p
                            :text (incr/incr get c :a)}}
                     {:dom {:tag :p
                            :text (incr/incr get c :b)}}
                     {:dom-events {:click {:event :button-clicked}}
                      :dom {:tag :button
                            :text "button"}}
                     ]}}))

(def d
  (incr/incr
    emit-widget
    {::w/parts @w/parts}
    app))

@d
(incr/stabilize!)

(js/setTimeout
  (fn []
    (reset! c {:a 1 :b 4 :c 1})
    ;(swap! app update-in [:dom :children :c :dom :text] #(str % "#"))
    (incr/stabilize!)
    )
  5000)

(js/setTimeout
  (fn []
    (reset! c {:a 1 :b 4 :c 2})
    ;(swap! app update-in [:dom :children :c :dom :text] #(str % "#"))
    (incr/stabilize!)
    )
  7000)

(set! js/onmessage
      (fn [e]
        (parts.events/dispatch!
          (t/read transit-r (.-data e)))
        (incr/schedule-stabilization!)))

;; for shared worker
#_(set! js/onconnect
      (fn [e]
        (let [port (aget (.-ports e) 0)]
          (.start port)
          (.postMessage port "asdf")
          (set! (.-onmessage port)
                (fn [e]
                  (.postMessage port "123")
                  )))))



