(ns editor.widgets
  (:require [runtime.widgets :as w]
            [incr.core :as incr]
            [parts.events :as e]
            ))

(def widgets
  {
   :editor-app
   {:set-styles {:height "100vh"}
    :locals {:widget-in-edit [:test-app :dom :children 2]}

    :docker-layout {:layout {:type :row
                             :content [{:type :component
                                        :id :editor-pane
                                        :width 80
                                        :componentName "frame"}
                                       {:type :component
                                        :id :properties-pane
                                        :componentName "frame"}]}
                    :frames {:editor-pane {:widget {:widget (w/gctx :params :app-in-edit)}}
                             :properties-pane {:widget-properties {:widget-in-edit (w/gctx :scope :widget-in-edit)}}}}}

   :button {:dom-events {:click (w/gctx :params :onclick)}
            :dom {:tag "vaadin-button"
                  :attrs {:theme (w/gctx :params :theme)}
                  :text "Button"}}

   :text-field {:dom-events {:input (w/gctx :params :oninput)}
                :dom {:tag "vaadin-text-field"
                      :attrs {:label (w/gctx :params :label)
                              :placeholder (w/gctx :params :placeholder)
                              :value (w/gctx :params :value)
                              }}}

   :v-layout {:set-styles {:display :flex
                           :flex-direction :column}
              :dom {:tag :div
                    :children (w/gctx :params)}}

   :h-layout {:set-styles {:display :flex
                           :flex-direction :row
                           :justify-content :space-between}
              :dom {:tag :div
                    :children (w/gctx :params)}}

   :widget-properties
   {:v-layout [{:dom {:tag :h5 :text "widget"}}
               {:set-styles {:font-size "var(--lumo-font-size-s)"
                             :font-family "Fira Code"}
                :dom {:tag :p :text (w/gctx :params :widget-in-edit)}}
               {:set-styles {:font-size "var(--lumo-font-size-s)"
                             :font-family "Fira Code"}
                :dom {:tag :p :text (w/with-ctx #(get-in (::w/widgets %) (get-in % [:params :widget-in-edit])))}}

               {:list-of {:items (w/with-ctx #(get-in (::w/widgets %) (get-in % [:params :widget-in-edit])))
                          :item-widget :part-properties-section
                          :param-for-key :part-id
                          :param-for-value :props
                          :common-params {:widget (w/gctx :params :widget-in-edit)}
                          }}

               ]}

   :part-properties-section
   {:events-handler {:field-changed (fn [event ctx]
                                      (if-let [v (try (cljs.reader/read-string (:event/value event))
                                                      (catch js/Error e nil))]
                                        [[:change-widget {:widget (get-in ctx [:params :widget])
                                                          :part (get-in ctx [:params :part-id])
                                                          :field (:field event)
                                                          :value v}]]))}
    :v-layout
    [{:dom {:tag :h5 :text (w/gctx :params :part-id)}}

     {:list-of {:items (w/gctx :params :props)
                :param-for-key :label
                :param-for-value :value
                :item-widget :default-prop-field
                }}
     ]}

   :default-prop-field
   {:text-field {:label (w/gctx :params :label)
                 :value (w/with-ctx (fn [ctx] (pr-str (get-in ctx [:params :value]))))
                 :oninput {:event :field-changed
                           :field (w/gctx :params :label)
                           }}}

   })


(e/register-effect-handler!
  :change-widget
  (fn [ctx args]
    (js/console.log "FX" :change-widget args)
    (swap! runtime.worker/widgets-cell
           assoc-in
           (concat (:widget args) [(:part args) (:field args)])
           (:value args)
           )

    )
  )