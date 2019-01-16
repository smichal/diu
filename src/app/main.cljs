(ns app.main
  (:require [core.incr :as incr]
            [mount.core :as mount :refer [defstate]]
            [core.engine :as e]
            core.events
            core.styles
            cljs.reader))


(def log js/console.log)

(defn value [x]
  (if (implements? IDeref x) @x x))

(declare update-widget! mouseover-widget widget-selected)

(def default-fields-editors
  {:string :string-prop-editor})

(def state-0
  (merge-with
    merge
    core.events/app-cfg
    core.styles/app-cfg
    {:parts
     {:w-p {:intercept {:after (fn [ctx params]
                                 (assoc ctx
                                   :dom {:tag :p
                                         :text (:text params)}))}
            :meta {:name "Text paragraph"
                   :doc ""
                   :props {:text {:label "Text"
                                  :doc ""
                                  :type :string}}
                   ;:part-editor  :default-part-editor
                   }}
      :input {:intercept {:after (fn [ctx params]
                                   (assoc ctx
                                     :dom {:tag :input
                                           :value (:value params)}))}}
      :dom {:intercept {:after (fn [ctx params]
                                 (assoc ctx
                                   :dom params))}}
      :container {:intercept {:after (fn [ctx params]
                                       (assoc ctx
                                         :dom {:tag :div
                                               :children
                                               (->> (if (map? (:children params))
                                                      (:children params)
                                                      (zipmap (range) (:children params)))
                                                    (map
                                                      (fn [[k v]]
                                                        [k (e/render-child k ctx v nil [:container :children k])]))
                                                    (into {}))
                                               #_(container-children (:children params))}))}}


      :list-of {:intercept {:after (fn [ctx params]
                                     (let [items (e/deref-one ctx (:items params))]
                                       ;(println "L" items params ctx)
                                       (when (map? items)
                                         (assoc ctx
                                           :dom {:tag :div
                                                 :children (fn [ctx]
                                                             (->> (e/deref-one ctx (:items params))
                                                                  (map
                                                                    (fn [[k _]]
                                                                      [k (e/render-child k #_(-> ctx
                                                                                                 (assoc :props (:common-props params))
                                                                                                 (assoc-in [:props (get params :key-field-name :key)] k)
                                                                                                 (assoc-in [:props (get params :val-field-name :val)] (get (:items params) k)))
                                                                                         ctx
                                                                                         (:item-widget params)
                                                                                         (assoc (:common-props params)
                                                                                           (get params :key-field-name :key) k
                                                                                           (get params :val-field-name :val) (get (:items params) k))
                                                                                         [:list-of :item-widget])]))
                                                                  (into {})))}))))}
                :meta {:name "List of"
                       :props {:item-widget {:label "Item widget"
                                             :type :string}
                               :items {:label "Items"
                                       :type :string}}}}
      :call {:intercept {:after (fn [ctx params]
                                  ;(println "CALL " params)
                                  (assoc ctx :dom
                                             (e/render-child "call" ctx (e/deref-one ctx (:widget params)) (:props params) [:call :widget])))}}

      :local-state {:intercept
                    {:before (fn [ctx params]
                               (println "Local state" params)
                               (let [ctx (reduce (fn [ctx [key val]]
                                                   (assoc ctx key (incr/cell val)))
                                                 ctx
                                                 params)
                                     ctx (core.events/add-handlers ctx {:set-local (fn [e]
                                                                                     (println "EVENT 1" e (get ctx (:key e)))
                                                                                     (when-let [cell (get ctx (:key e))]
                                                                                       (incr/cell-set! cell (:val e))))})]
                                 ctx))
                     ;:after
                     #_(fn [ctx params]
                         (reduce (fn [ctx [key _]]
                                   (dissoc ctx key))
                                 ctx
                                 params))}}

      :locals {:intercept (fn [ctx params]
                            ;(println "Locals" params)
                            (reduce (fn [ctx [key val]]
                                      (assoc-in ctx [:locals key] val))
                                    ctx
                                    params))}


      :w {:w-p {:text "abcd"}
          :set-styles {:color "red"}
          :order [:set-styles :w-p]}
      :w2 {:w-p {:text (fn [ctx] (str "abcd " (:idx ctx)))}
           :dom-events {:click (fn [ctx] {:event :event1
                                          :what (:idx ctx)})}
           :order [:dom-events :w-p]}
      :w11 {:container {:children {0 :w
                                   1 {:container {:children (->> (conj (vec (repeat 5 :w2)) :w)
                                                                 (zipmap (range)))}}}}
            :events-handler {:event1 (fn [ctx]
                                       (fn [e]
                                         (println e)
                                         ;(incr/cell-swap! state-cell #(assoc-in % [:parts :w :set-styles :color] "blue"))
                                         ))}}

      :app-preview {:w-p {:text "preview"}
                    :set-styles {:color "#888"}
                    :order [:set-styles :w-p]}

      :test-items {:list-of {:items {0 0 1 1 2 42}
                             :item-widget :test-list-item}}
      :test-list-item {:w-p {:text #(str "a" (get-in % [:props :val]))}}
      :test-list-item2 {:w-p {:text #(str "b" (get-in % [:props :val]))}}

      :w1 {:set-styles {:display :grid
                        :gridTemplateColumns "1fr 2fr 300px"
                        :height "100vh"}
           :local-state {:in-edit [:app-preview]
                         :frame-rect [0 0 0 0]
                         :mouseover-widget []}
           :container
           {:children {:a {:container {:children [{:w-p {:text "=> a"}
                                                   :dom-events {:click {:event :set-local
                                                                        :key :in-edit
                                                                        :val [:test-items]}}
                                                   :order [:dom-events :w-p]}
                                                  {:w-p {:text "=> b"}
                                                   :dom-events {:click {:event :set-local
                                                                        :key :in-edit
                                                                        :val [:app-preview]}}
                                                   :order [:dom-events :w-p]}]}}
                       :left {:container {:children {:0 :app-preview
                                                     :1 :test-items}}
                              :dom-events {:mouseover {:event :mouseover-widget}
                                           :click {:event :widget-selected}}
                              :order [:dom-events :container]}
                       :right {:widget-editor {:in-edit #(get % :in-edit) #_:test-items}}

                       :frame :hover-frame

                       }}
           :events-handler {:mouseover-widget #(do mouseover-widget)
                            :widget-selected #(do widget-selected)}
           :order [:local-state :set-styles :events-handler :container]
           }

      :hover-frame {:set-styles {:position :absolute
                                 :border "1px solid #00e"
                                 :z-index -1
                                 :top (fn [ctx] (get @(ctx :frame-rect) 0))
                                 :left (fn [ctx] (get @(ctx :frame-rect) 1))
                                 :width (fn [ctx] (get @(ctx :frame-rect) 2))
                                 :height (fn [ctx] (get @(ctx :frame-rect) 3))
                                 }
                    :dom {:tag :div}}

      :widget-editor {;:locals {:in-edit #(deref (get-in % [:props :in-edit]))}
                      :container
                      {:children
                       [{:w-p {:text #(str "props of " @(get-in % [:props :in-edit]))}}
                        {:dom {:tag :hr}}
                        {:list-of {:items #(get-in % (cons :parts @(get-in % [:props :in-edit])))
                                   :common-props {:widget #(deref (get-in % [:props :in-edit]))}
                                   :key-field-name :part
                                   :val-field-name :part-props
                                   :item-widget :part-editor-wrapper}}]}
                      :events-handler {:update-widget #(do update-widget!)}
                      :set-styles {:background-color "rgba(239,237,231,.5)"
                                   :border-left "1px solid #cec9c4"
                                   :padding "0 10px"}
                      :order [:set-styles :events-handler :container]
                      }

      :part-editor-wrapper {:container
                            {:children
                             [{:w-p {:text (fn [ctx]
                                             (str (get-in ctx [:props :part])
                                                  " "
                                                  @(get-in ctx [:parts (get-in ctx [:props :part]) :meta :name])
                                                  ))}}
                              #_{:list-of {:item-widget :part-prop-editor #_:default-prop-editor
                                           :items #(get-in % [:props :part-props])
                                           :common-props {:widget #(get-in % [:props :widget])
                                                          :part #(get-in % [:props :part])}}}
                              #_{:call {:widget :default-part-editor
                                        :params {:widget #(get-in % [:props :widget])
                                                 :part #(get-in % [:props :part])}}}
                              {:default-part-editor {:widget #(get-in % [:props :widget])
                                                     :part #(get-in % [:props :part])}}
                              {:dom {:tag :hr}}
                              ]}}

      :default-part-editor {:container
                            {:children
                             [{:list-of {:item-widget :part-prop-editor #_:default-prop-editor
                                         :items #(get-in % [:parts (get-in % [:props :part]) :meta :props])
                                         :key-field-name :field-name
                                         :val-field-name :field-data
                                         :common-props {:widget #(get-in % [:props :widget])
                                                        :part #(get-in % [:props :part])}}}]}}

      :part-prop-editor {:container
                         {:children
                          [{:w-p {:text #(str @(get-in % [:props :field-data :label]) " <" (get-in % [:props :field-name]) ">")}}
                           {:call
                            {:widget #(get default-fields-editors @(get-in % [:props :field-data :type]))
                             :props {:widget #(get-in % [:props :widget])
                                     :part #(get-in % [:props :part])
                                     :field #(get-in % [:props :field-name])
                                     }}
                            }
                           ]}}

      :log-params {:w-p {:text #(:props %)}}
      :string-prop-editor {:input {:value #(incr/fmap pr-str
                                                      (get-in % (concat [:parts] (get-in % [:props :widget]) [(get-in % [:props :part]) (get-in % [:props :field])])))}
                           :dom-events {:keyup {:event :update-widget
                                                :widget #(get-in % [:props :widget])
                                                :part #(get-in % [:props :part])
                                                :field #(get-in % [:props :field])
                                                }}
                           :order [:dom-events :input]}



      :default-prop-editor {:container
                            {:children
                             [{:w-p {:text #(str (get-in % [:props :key]))}}
                              {:input {:value #(incr/fmap pr-str (get-in % [:props :val]))}
                               :dom-events {:keyup {:event :update-widget
                                                    :widget #(get-in % [:props :widget])
                                                    :part #(get-in % [:props :part])
                                                    :field #(get-in % [:props :key])
                                                    }}
                               :order [:dom-events :input]}]}}
      }}))

(def state-cell (incr/cell state-0))

(def ctx-0
  (with-meta
    {:parts (get state-cell :parts)}
    {:incr/name "N"}))

(defn update-widget! [e]
  (incr/cell-swap! state-cell (fn [m]
                                (assoc-in m (concat [:parts ] (:widget e) [(:part e) (:field e)])
                                          (cljs.reader/read-string (:event/value e)))))
  (println "update-widget!" e))

(defn mouseover-widget [e ctx]
  (let [{:strs [top left width height]} (:event/bounding-rect e)
        widget-path (:widget/path (@e/name->ctx (:event/target-id e)))
        ]
    (incr/cell-swap! (:frame-rect ctx)
                     #(do [top left width height]))
    (incr/cell-swap! (:mouseover-widget ctx) (fn [] widget-path))
    ;(println "mouseover-widget" widget-path e)
    ))

(defn widget-selected [e ctx]
  ;(println "widget-selected" e @(:mouseover-widget ctx))
  (incr/cell-swap! (:in-edit ctx) (fn [] @(:mouseover-widget ctx)))
  )

(defn tt []
  (let [rw (e/render-child "" ctx-0 :w1 nil [])
        fd (e/flat-dom rw)
        obs (incr/diff-thunk (fn [] (print "delta " @fd)))
        ]
    (incr/add-parent! obs (incr/Observer.))
    (incr/stabilize!)
    (js/console.log rw)
    @rw))



(comment
  (defn ^:dev/after-load start []
    (js/console.log "start")

    ;(incr/cell-set! state-cell state-0)
    ;(incr/stabilize!)

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
    (js/console.log "stop")))
