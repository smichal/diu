(ns editor.part-editor
  (:require [runtime.widgets :as w]
            [incr.core :as incr]
            [parts.events :as e]
            [runtime.expr :refer [expr]]
            cljs.reader
            editor.expr-blocks
            ))

(def widgets
  {
   ::part-properties
   {:locals {:part-id (w/gctx :params :part-id)
             :part (expr '(ctx ::w/parts (ctx :params :part-id)))
             :widget-path (expr '(ctx :params :widget))
             :widget (expr '(get-in (ctx ::w/widgets) (ctx :params :widget)))
             }
    :events-handler {:field-changed (fn [event ctx]
                                      (if-let [v (try (cljs.reader/read-string (:event/value event))
                                                      (catch js/Error _ nil))]
                                        (do
                                          [[:change-widget {:widget (get-in ctx [:scope :widget-path])
                                                            :part (get-in ctx [:scope :part-id])
                                                            :field (:field event)
                                                            :value v}]])))
                     :expr-changed (fn [event ctx]
                                     [[:change-widget {:widget (get-in ctx [:scope :widget-path])
                                                       :part (get-in ctx [:scope :part-id])
                                                       :field (get-in ctx [:scope :param-id])
                                                       :value (or (:value event) (:event/value event))}]])
                     }
    :set-styles {:margin "16px 0"}
    :v-layout
    {:children
     [{:set-styles {:border-top "1px solid var(--lumo-contrast-10pct)"
                    :padding "16px 12px"}
       :h-layout
       {:children
        [{:set-styles {:margin 0
                       :color "var(--lumo-body-text-color)"
                       :font-weight 500}
          :dom {:tag :h4 :text (expr '(or (ctx :scope :part :part/name) (ctx :scope :part-id)))}}
         {:set-styles {:color "var(--lumo-body-text-color)"
                       :margin 0}
          :dom {:tag :h4 :text "+"}}
         ]}}

      {:set-styles {:padding "0 12px"}
       :dom
       {:children
        [(w/with-ctx
           #(editor.expr-blocks/expr-field
              (-> %
                  (get-in [:scope :widget])
                  incr/value
                  (get-in [(get-in % [:scope :part-id])]))
              (-> %
                  (get-in [:scope :part])
                  incr/value
                  (get-in [:part/params]))
              %))]}}

      ]}}

   ::field
   {:set-styles {:flex-wrap :wrap
                 :margin "var(--lumo-space-s) 0"}
    :h-layout
    {:children
     [{:set-styles {:color "var(--lumo-secondary-text-color)"
                    :font-weight 500
                    :font-size "var(--lumo-font-size-s)"
                    :align-self :center
                    :margin-bottom 3
                    :margin-right 6
                    :min-width 60
                    }
       :dom {:tag :div
             :text '(params :label)}}
      '(params :field)
      ]}
    }

   ;:expr-input
   #_{:set-styles {:flex-grow 1}
    :dom {:children [(w/with-ctx
                       #(editor.expr-blocks/expr-input
                          (-> %
                              (get-in [:scope :widget])
                              incr/value
                              (get-in [(get-in % [:scope :part-id]) (get-in % [:scope :param-id])]))
                          (-> %
                              (get-in [:scope :part])
                              incr/value
                              (get-in [:part/params (get-in % [:scope :param-id])]))
                          %))]}}

   ;:param-editor
   #_{:locals {:param-id (w/gctx :params :item :param)}
    ::field
    {:label (w/gctx :params :item :label)
     :field {:expr-input {}}}
    }

   })


(e/register-effect-handler!
  :change-widget
  (fn [ctx args]
    (let [path (vec (concat (incr/value (:widget args)) [(:part args) #_(:field args)]))]
      ;(js/console.log "FX" :change-widget args path)
      (swap! runtime.worker/widgets-cell
             assoc-in
             path
             (w/with-path-annotation
               path
               (:value args))
             ))))
