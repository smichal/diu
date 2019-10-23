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
    :v-layout
    [{:set-styles {:padding "5px 12px"
                   :border-top "1px solid var(--lumo-contrast-10pct)"}

      :dom {:tag :h5 :text (expr '(or (ctx :scope :part :part/name) (ctx :scope :part-id)))}}

     {:set-styles {:padding "0 12px"}
      :list-of {:items (expr '(ctx :scope :part :part/params))
                :param-for-key :param-id
                :param-for-value :desc
                :item-widget :param-editor
                }}
     ]}

   ;:default-prop-field
   #_{:text-field {:label (w/gctx :params :label)
                   :value (w/with-ctx (fn [ctx] (pr-str (get-in ctx [:params :value]))))
                   :oninput {:event :field-changed
                             :field (w/gctx :params :label)
                             }}}

   :param-editor
   {:locals {:param-id (w/gctx :params :param-id)}
    :dom {:children
          [{:text-field {:label (w/gctx :params :desc :param/name)
                         :value (expr '(pr-str (get-in (ctx :scope :widget) [(ctx :scope :part-id) (ctx :params :param-id)])))
                         #_(w/with-ctx (fn [ctx] (pr-str (get-in ctx [:params :value]))))
                         :oninput {:event :field-changed
                                   :field (w/gctx :params :param-id)
                                   }}}


           (w/with-ctx
             #(editor.expr-blocks/string-input
                (-> %
                    (get-in [:scope :widget])
                    incr/value
                    (get-in [(get-in % [:scope :part-id]) (get-in % [:scope :param-id])])
                    )
                #_(get-in (ctx :scope :widget) [(ctx :scope :part-id) (ctx :params :param-id)])
                %))

           ]}}

   })


(e/register-effect-handler!
  :change-widget
  (fn [ctx args]
    ;(js/console.log "FX" :change-widget args)
    (swap! runtime.worker/widgets-cell
           assoc-in
           (concat (incr/value (:widget args)) [(:part args) (:field args)])
           (:value args)
           )))
