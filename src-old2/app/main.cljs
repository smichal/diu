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


(def log js/console.log)
;(def log (constantly nil))

(defprotocol IQuery
  (-query [this ctx]))

(defn value [x]                                             ;; xxx
  (if (implements? IDeref x) @x x))

(defn do-query [ctx q]
  (if (implements? IQuery q)
    (-query q ctx)
    q))

(defn r-do-query [ctx q]
  (cond
    (map? q) (sp/transform [sp/MAP-VALS] (partial r-do-query ctx) q)
    (vector? q) (sp/transform [sp/ALL] (partial r-do-query ctx) q)
    :else (do-query ctx q)))

(defn r-do-value-query [ctx q]
  (cond
    (map? q) (sp/transform [sp/MAP-VALS] #(value (r-do-query ctx %)) q)
    (vector? q) (sp/transform [sp/ALL] #(value (r-do-query ctx %)) q)
    :else (do-query ctx q)))

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
        (log "from-ctx" path res)
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
                          ;(log "from-db" e a "=>" (if res @res "NIL!"))
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
  (events/dispatch! ctx event))

(defn widget-call [c ctx]
  (let [c (value (do-query ctx c))                          ;; value?
        _ (log "widget-call" c)
        [w p] (if (vector? c) c [c])]
    {:widget w
     :params (sp/transform [sp/MAP-VALS] (partial do-query ctx) p)
     :ctx ctx}))

(defn w-container [w p]
  (assoc w
    :render (fn [ctx]
              {:tag :div
               :children (map #(widget-call % ctx)
                              ; (value ...)
                              (do-query ctx (:children p)))})))


(defn w-button [w p]
  (assoc w
    :render (fn [ctx]
              {:tag :button
               :children {:text (do-query ctx (:text p))}})))

(defn w-p [w p]
  (assoc w
    :render (fn [ctx]
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
                             (log "list-of item" (:field-name p) x)
                             (widget-call (:item-widget p)
                                          (assoc ctx (:field-name p) x))
                             #_{:widget (:item-widget p)
                              :ctx (assoc ctx (:field-name p) x)})
                           (value (do-query ctx (:items p))))})))

(defn w-if [w p]
  (assoc w
    :render (fn [ctx]

              (log "RENDER IF")
              (if (value (do-query ctx (:cond p)))
                (do
                  (log "IF THEN" (value (do-query ctx (:then p))))
                  (widget-call (:then p) ctx)
                  #_{:widget (value (do-query ctx (:then p)))
                   ;:params (sp/transform [sp/MAP-VALS] (partial do-query ctx) p) #_p
                   :ctx ctx})
                (widget-call (:else p) ctx)
                #_{:widget (value (do-query ctx (:else p)))
                 ;:params (sp/transform [sp/MAP-VALS] (partial do-query ctx) p) #_p
                 :ctx ctx}
                ))))

(defn w-or [w p]
  (assoc w
    :render (fn [ctx]
              (if-let [a (value (do-query ctx (:a p)))]
                (widget-call a ctx)
                #_{:widget a
                 :ctx ctx}
                (widget-call (:b p) ctx)
                #_{:widget (value (do-query ctx (:b p)))
                 :ctx ctx}
                ))))

(defn w-with-state [w p]
  (update w :render
          (fn [f]
            (fn [ctx]
              ;(log "WITH STATE RID" (::dom/rid ctx))
              (let [ctx
                    (if-let [x (:store-rid-as p)]
                      (assoc ctx x (::dom/rid ctx))
                      ctx)]
                (ds/tx-effect-handler [{:winst/rid (::dom/rid ctx)}])
                (f ctx))))))

#_(defn new-local-state [ctx]
  (get-in
    (ds/tx-effect-handler [{:db/id -1}])
    [:tempids -1]))

(defn dom-event-handler [map-or-fn ctx dom-event]
  (doseq [map-or-fn (if (vector? map-or-fn) map-or-fn [map-or-fn])]
    (dispatch! ctx
               (if (map? map-or-fn)
                 ;(sp/transform [sp/MAP-VALS] #(value (do-query (assoc ctx ::dom-event event) %)) map-or-fn)
                 (r-do-value-query (assoc ctx ::dom-event dom-event) map-or-fn)
                 (map-or-fn dom-event ctx)))))

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


(defn events-dispatcher [w p]
  (update w :context #(events/events-scope % p))


  #_(let [ec (events/new-event-context nil)]
    (doseq [[name handler] p]
      (events/register-event-handler! ec name handler))

    #_(assoc-in w
              [:context :dispatcher]
              (fn [event]
                (js/console.log "EVENT" event)
                (events/dispatch! ec event)))))

(defn tx-effect [w p]
  (assoc-in w [:context ::events/effects :tx] ds/tx-effect-handler))

(defn register []
  ;(mount/start)

  (ds/tx-effect-handler [{:db/id 10 :some/val "v0"}])
  (js/console.log "entity" (:some/val (d/entity (ds/db) 10)))

  (let [text-node (insert! [[w-set-styles {:color "#f00" :font-size "20px"}]
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
              (insert! [[w-react-on-event {"click" {:event :show-popup
                                                    :widget text-node}
                                           #_{:event :click}}]
                        [w-button {:text "button1"}]] :button1)
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

        step-props (insert! [[w-container
                              {:children
                               [(insert! [[w-list-of {:items (from-ctx :step :widget-step/params)
                                                      :item-widget step-param
                                                      :field-name :kv}]])

                                (insert! [[w-p {:text (cmp #(str "params: " (pr-str (deref %)))
                                                           ;; db not ctx
                                                           (from-db (from-ctx :step) :widget-step/params)
                                                           #_(from-ctx :step :widget-step/params))}]])
                                ]}]] :step-props)

        step (insert! [[w-set-styles {:border "1px solid #555"
                                      :margin-bottom "5px"}]
                       [w-container
                        {:children [(insert! [[w-set-styles {:border-bottom "1px solid #555"}]
                                              [w-p {:text (cmp #(str "fn: " (.-name %))
                                                               (from-ctx :step :widget-step/fn))}]])
                                    (insert! [[w-or {:a (from-db [:step-editor/step-fn (from-ctx :step :widget-step/fn)] :step-editor/widget)
                                                     :b step-props}]])]}]])

        _ (ds/tx-effect-handler
            [{:step-editor/step-fn w-set-styles
              :step-editor/widget (insert! [[w-container
                                             {:children
                                              [(insert! [[w-p {:text "Styles"}]])
                                               step-props]
                                              }]])}])

        ;step
        #_(insert! [[w-container
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


        add-step-list (insert!
                        [[w-list-of {:items (from-db-query-attr :step-editor/step-fn)
                                     :field-name :step
                                     :item-widget (insert!
                                                    [[w-react-on-event {"click" [{:event :add-step-fn
                                                                                  :step-fn (from-ctx :step)
                                                                                  :widget (from-ctx :params :widget)}
                                                                                 {:event :close-popup}]}]
                                                     [w-p {:text (cmp
                                                                   #(.-name %)
                                                                   (from-ctx :step))}]])
                                     }]])


        props (insert!
                [[w-set-styles {:color "#fff"
                                :background "#333"
                                :padding "20px"
                                :font-family "Fira Code"
                                :font-size "11px"
                                :width "260px"
                                }]
                 [w-container
                  {:children
                   [(insert! [[w-p {:text (from-db (from-ctx :params :widget) :widget/name)}]])
                    (insert! [[w-list-of {:items (from-db (from-ctx :params :widget) :widget/steps)
                                          :item-widget step
                                          :field-name :step
                                          }]] :props)
                    (insert! [[w-react-on-event {"click" {:event :show-popup
                                                          :widget [add-step-list {:widget (from-ctx :params :widget)}]}}]
                              [w-p {:text "Add step"}]])]}]])

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


        popup (insert!
                [[w-if {:cond (from-db [:winst/rid (from-ctx :wrapper-rid)] :app/popup)
                        :then (insert! [[w-container
                                         {:children
                                          [(insert! [[w-set-styles {:position :absolute
                                                                    :top "0"
                                                                    :left "0"
                                                                    :right "0"
                                                                    :bottom "0"
                                                                    :background "rgba(0,0,0,0.5)"}]
                                                     [w-react-on-event {"click" {:event :close-popup}}]
                                                     [w-container {:children
                                                                   [(insert! [[w-set-styles {:position :absolute
                                                                                             :top "40%"
                                                                                             :left "calc(50% - 150px)"
                                                                                             :width "300px"
                                                                                             :background "#333"
                                                                                             :padding "10px 20px"
                                                                                             :box-shadow "0 0 25px 0 #555"}]
                                                                              [w-container {:children [(from-db [:winst/rid (from-ctx :wrapper-rid)] :app/popup)]}]])]
                                                                   }]])
                                           ]}]])}]]
                )


        editor-wrapper (insert!
                         [[events-dispatcher
                           {:click (fn []
                                     {:tx [[:db/add 10 :some/val (str "t-" (.getTime (js/Date.)))]]})
                            :params-changed (fn [{:keys [step key val] :as event}]
                                              ;(js/console.log "event" event)
                                              (let [v (:widget-step/params (d/entity (ds/db) (:db/id step)))]
                                                {:tx [[:db/add (:db/id step) :widget-step/params (assoc v key val)]]}))
                            :select-widget (fn [{:keys [widget wrapper]}]
                                             {:tx [{:winst/rid wrapper
                                                    :wrapper/for-edit [:widget/name widget]}]})

                            :close-popup (fn [_ ctx]
                                           {:tx [[:db.fn/retractAttribute [:winst/rid (:wrapper-rid ctx)] :app/popup]]})
                            :show-popup (fn [{:keys [widget]} ctx]
                                          {:tx [{:winst/rid (:wrapper-rid ctx)
                                                 :app/popup widget}]})

                            :add-step-fn (fn [{:keys [widget step-fn]} _]
                                           {:tx [{:db/id -1
                                                  :widget-step/fn step-fn
                                                  :widget-step/params {:width "200px"}}
                                                 [:db/add (:db/id widget) :widget/steps -1
                                                  #_[:step-editor/step-fn step-fn]]
                                                 ]}
                                           )

                            }]
                          [tx-effect {}]
                          [w-with-state {:store-rid-as :wrapper-rid}]
                          [w-container
                           {:children
                            [popup
                             (insert!
                               [[w-set-styles {:display :flex
                                               :align-items :stretch
                                               :height "100vh"}]
                                [w-container
                                 {:children
                                  [widgets-list
                                   app
                                   [props {:widget (from-db [:winst/rid (from-ctx :wrapper-rid)] :wrapper/for-edit)}]]}]])]}]]
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
