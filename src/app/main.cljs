(ns app.main
  (:require
    [core.ds :as ds]
    [core.widgets :as w]
    [core.dom :as dom]
    [core.computed :as c]
    [core.events :as events]
    [mount.core :as mount :refer [defstate]]
    [datascript.core :as d]
    [com.rpl.specter :as sp]
    [oops.core :refer [oget oset! ocall oapply ocall! oapply!
                       oget+ oset!+ ocall+ oapply+ ocall!+ oapply!+]]
    ))


(defprotocol IQuery
  (-query [this ctx]))

(defn do-query [ctx q]
  (if (implements? IQuery q)
    (-query q ctx)
    q))

(defn r-do-query [ctx q]
  (cond
    (map? q) (sp/transform [sp/MAP-VALS] (partial r-do-query ctx) q)
    (vector? q) (sp/transform [sp/ALL] (partial r-do-query ctx) q)
    :else (do-query ctx q)))

(defn value [x]                                             ;; xxx
  (if (implements? IDeref x) @x x))

(defn from-ctx [& path]
  (reify
    IQuery
    (-query [_ ctx]
      #_(get-in ctx path)
      (let [res
            (reduce (fn [a b]
                      ;(println a b (map? a) (b a))
                      (cond
                        (or (fn? b) (implements? IFn b)) (b a)
                        (implements? IFn a) (a b)
                        (object? a) (oget+ a b)
                        ;; xxx
                        ))
                    ctx
                    (r-do-query ctx path))]
        (js/console.log "from-ctx" path res)
        res))
    Object
    (toString [this] (str "[ctx=> " path "]"))))

(defn from-db [eid attr]
  (reify
    IQuery
    (-query [_ ctx]
      ;(c/computed (fn []  ;; xxx: because deref
                    (let [e (value (r-do-query ctx eid))
                          a (value (r-do-query ctx attr))]
                      (when (and e a)
                        (let [res (ds/sub-entity-attr e a)]
                          (js/console.log "from-db" e a "=>" @res)
                          res))))
    Object
    (toString [this] (str "[db=> E: " eid " A: " attr "]"))))

(defn from-db-query-attr [attr]
  (reify
    IQuery
    (-query [_ ctx] (ds/sub-attr (value (r-do-query ctx attr))))
    Object
    (toString [this] (str "[db=> A: " attr "]"))))

(defn cmp [f & qs]
  (reify
    IQuery
    (-query [_ ctx] (apply f (map (partial do-query ctx) qs)))))

(defn computed [f]
  (reify
    IQuery
    (-query [_ ctx] (c/computed (f ctx)))))


(defn dispatch! [ctx event]
  ;(js/console.log "ctx" ctx)
  ((:dispatcher ctx) event)
  )

(defn w-container [w p]
  (assoc w
    :render (fn [ctx]
              {:tag :div
               :children (map (fn [c]
                                (let [[w p] (if (vector? c) c [c])]
                                  {:widget w
                                   :params (sp/transform [sp/MAP-VALS] (partial do-query ctx) p) #_p
                                   :ctx ctx}))
                              ; (value ...)
                              (do-query ctx (:children p)))})))


(defn w-button [w p]
  (assoc w
    :render (fn [ctx]
              {:tag :button
               :events {"click" (fn [e] (dispatch! ctx {:event :click}))}
               :children {:text (do-query ctx (:text p))}})))

(defn w-p [w p]
  (assoc w
    :render (fn [ctx]
              (js/console.log "AAAAAA w-p" ctx)
              {:tag :p
               :children {:text (do-query ctx (:text p))}})))

(defn w-set-styles [w p]
  (update w :render
          (fn [f]
            (fn [ctx]
              ;(js/console.log "styles" p)
              (update
                (f ctx)
                :styles
                merge
                (sp/transform [sp/MAP-VALS] (partial do-query ctx) p)
                )))))

(defn w-list-of [w p]
  (assoc w
    :render (fn [ctx]
              {:tag :div
               :children (mapv
                           (fn [x]
                             {:widget (:item-widget p)
                              :ctx (assoc ctx (:field-name p) x)})
                           (value (do-query ctx (:items p))))})))

(defn w-with-state [w _]
  (update w :render
          (fn [f]
            (fn [ctx]
              ;(js/console.log "styles" p)
              (ds/tx-effect-handler [{:winst/rid (::dom/rid ctx)}])
              (f ctx)))))

#_(defn new-local-state [ctx]
  (get-in
    (ds/tx-effect-handler [{:db/id -1}])
    [:tempids -1]))

(defn dom-event-handler [map-or-fn ctx event]
  (dispatch! ctx
             (if (map? map-or-fn)
               (sp/transform [sp/MAP-VALS] #(do-query (assoc ctx ::dom-event event) %) map-or-fn)
               (map-or-fn ctx event))))

(defn w-react-on-event [w p]
  (update w :render
          (fn [f]
            (fn [ctx]
              (update
                (f ctx)
                :events
                merge
                (sp/transform [sp/MAP-VALS] #(fn [e] (dom-event-handler % ctx e)) p)
                )))))

(defn w-input [w p]
  (assoc w
    :render (fn [ctx]
              {:tag :input
               :attrs {:value (do-query ctx (:value p))}})))

(defn insert! [steps & [name]]
  (get-in
    (ds/tx-effect-handler [(assoc (w/compose-for-db steps name) :db/id -1)])
    [:tempids -1]))

(js/console.log (insert! [[w-button {:text "asd"}]]))


(defn events-dispatcher [w p]
  (let [ec (events/new-event-context nil)]
    (doseq [[name handler] p]
      (events/register-event-handler! ec name handler))

    (assoc-in w
              [:context :dispatcher]
              (fn [event]
                (js/console.log "EVENT" event)
                (events/dispatch! ec event)))))


(defn register []
  ;(mount/start)

  (ds/tx-effect-handler [{:db/id 10 :some/val "v0"}])
  (js/console.log "entity" (:some/val (d/entity (ds/db) 10)))

  (let [
        text-node (insert! [[w-set-styles {:color "#f00" :font-size "20px"}]
                            [w-p {:text (from-db 10 :some/val)}]]
                           :text-node)
        app
        (insert!
          [[w-set-styles {:flex 1
                          :margin "10px"}]
           [w-container
            {:children
             [;[:widget/name :base]
              text-node
              text-node
              (insert! [[w-input {}]])
              (insert! [[w-button {:text "button1"}]] :button1)
              ]}]]
          :app)

        step-param (insert! [[w-set-styles {:display :flex}]
                             [w-container
                              {:children
                               [(insert! [[w-set-styles {:flex 1}]
                                          [w-p {:text (from-ctx :kv 0)}]])
                                (insert! [[w-set-styles {:flex 1}]
                                          [w-react-on-event {"input" {:event :params-changed
                                                                      :step (from-ctx :step)
                                                                      :key (from-ctx :kv 0)
                                                                      :val (from-ctx ::dom-event #(.-value (.-target %)))}}]
                                          [w-input {:value (from-ctx :kv 1)
                                                    ;:on-input
                                                    #_(cmp
                                                        (fn [step k] {:event :params-changed :step step :key k})
                                                        (from-ctx :step)
                                                        (from-ctx :kv 0))}]] :step-input)
                                ]}]]
                            :step-param)

        step (insert! [[w-container
                        {:children
                         [(insert! [[w-p {:text (cmp #(str "fn: " (.-name %))
                                                     (from-ctx :step :widget-step/fn))}]])
                          (insert! [[w-list-of {:items (from-ctx :step :widget-step/params)
                                                :item-widget step-param
                                                :field-name :kv}]])

                          (insert! [[w-p {:text (cmp #(str "params: " (pr-str (deref %)))
                                                     ;; db not ctx
                                                     (from-db (from-ctx :step) :widget-step/params)
                                                     #_(from-ctx :step :widget-step/params))}]])
                          ]}]] :step)

        props (insert! [[w-set-styles {:color "#fff"
                                       :background "#333"
                                       :padding "20px"
                                       :font-family "Fira Code"
                                       :font-size "11px"
                                       :width "260px"
                                       }]
                        #_[w-p {:text (from-db (from-ctx :params :widget) :widget/steps)}]
                        [w-list-of {:items (from-db (from-ctx :params :widget) :widget/steps)
                                    :item-widget step
                                    :field-name :step
                                    }]] :props)

        widget-list-item (insert! [[w-react-on-event {"click" {:event :select-widget
                                                               :widget (from-ctx :widget)
                                                               :wrapper 1 ;(from-ctx ::dom/parent)
                                                               }}]
                                   [w-p {:text (from-ctx :widget)}]] :widget-list-item)

        widgets-list (insert! [[w-set-styles {:color "#fff"
                                              :background "#333"
                                              :padding "20px"
                                              :font-family "Fira Code"
                                              :font-size "11px"
                                              }]
                               [w-list-of {:items (from-db-query-attr :widget/name)
                                           :item-widget widget-list-item
                                           :field-name :widget}]] :widgets-list)

        editor-wrapper (insert!
                         [[events-dispatcher
                           {:click (fn []
                                     {:tx [[:db/add 10 :some/val (str "t-" (.getTime (js/Date.)))]]})
                            :params-changed (fn [_ {:keys [step key val] :as event}]
                                              ;(js/console.log "event" event)
                                              (let [v (:widget-step/params (d/entity (ds/db) (:db/id step)))]
                                                {:tx [[:db/add (:db/id step) :widget-step/params (assoc v key val)]]}))
                            :select-widget (fn [_ {:keys [widget wrapper]}]
                                             {:tx [{:winst/rid wrapper
                                                    :wrapper/for-edit [:widget/name widget]}]})
                            }]
                          [w-with-state {}]
                          [w-set-styles {:display :flex
                                         :align-items :stretch
                                         :height "100vh"}]
                          [w-container
                           {:children
                            [widgets-list
                             app
                             [props {:widget (from-db [:winst/rid (from-ctx ::dom/rid)] :wrapper/for-edit)}]]}]]
                         :editor-wrapper)
        ]

    (c/reaction
      (fn []
        (js/console.log "AAA")
        (dom/render-children! js/document.body [{:widget editor-wrapper :ctx {}}])))))



(defn ^:dev/after-load start []
  (js/console.log "start")
  )

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (js/console.log "init")

  (register)

  (start))

;; this is called before any code is reloaded
(defn ^:dev/before-load stop []
  (js/console.log "stop"))
