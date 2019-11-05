(ns editor.widgets
  (:require [runtime.widgets :as w]
            [incr.core :as incr]
            [parts.events :as e]
            ))

(defn search-filter [phrase items]
  ;(js/console.log "search-filter" phrase items)
  (->> items
       (filterv #(clojure.string/includes? (clojure.string/lower-case (:name %))
                                           (clojure.string/lower-case phrase)))))

(defn search-input-key-event [event ctx]
  (let [items-num (count (incr/value (get-in ctx [:scope :items])))
        curr @(get-in ctx [:scope :active-item])]
    ;; fixme: count after filter
    (case (:event/key-pressed event)
      "ArrowUp" [:set-local :active-item (max 0 (dec curr))]
      "ArrowDown" [:set-local :active-item (min (inc curr) (dec items-num))]
      #_"Enter" #_[[:dispatch-event (assoc (get-in ctx [:props :on-select])
                                      :item selected)]
                   [:set-local :popup false]]
      nil)))

(defn dialog-open-event [event _]
  [[:set-local :dialog (:event/value event)]])

(defn parts-for-search [parts]
  (->> parts
       (map (fn [[k v]]
              {:name (or (:part/name v) (str k))
               :desc (or (:part/desc v) "")}
              ))))

(def widgets
  {
   :editor-app
   {:set-styles {:height "100vh"}
    :local-state {:widget-in-edit [:test-app :dom :children 2]
                  :ctx-of-widget-in-edit nil}

    :events-handler {:select-widget (fn [event ctx]
                                      (if (::w/instance-path event)
                                        (let [call-id (hash (::w/instance-path event))
                                              widget-ctx (@w/call-id->ctx call-id)]
                                          (if widget-ctx
                                            [[:set-local :widget-in-edit (::w/param-path widget-ctx)]
                                             [:set-local :ctx-of-widget-in-edit widget-ctx]]
                                            (js/console.warn "widget not found" call-id (::w/instance-path event))))

                                        (when (:event/meta-key event)
                                          (let [call-id (-> (:event/target-call-id event)
                                                            (clojure.string/split " ")
                                                            first ;last
                                                            js/parseInt)
                                                widget-ctx (@w/call-id->ctx call-id)]
                                            [[:set-local :widget-in-edit (::w/param-path widget-ctx)]
                                             [:set-local :ctx-of-widget-in-edit widget-ctx]
                                             ]))))
                     :undo (fn [e ctx]
                             (js/console.log "UNDO" (count @runtime.worker/history-atom))
                             (when-let [w (last (butlast @runtime.worker/history-atom))]
                               (reset! runtime.worker/history-atom
                                       (vec (butlast @runtime.worker/history-atom)))
                               (reset! runtime.worker/widgets-cell w)))
                     }
    :v-layout
    {:children
     [{:set-styles {:background "var(--lumo-contrast-5pct)"}
       :h-layout
       {:children
        [{:button {:icon "lumo:undo"
                   :theme "contrast tertiary"
                   :onclick {:event :undo}}}
         ]}}
      {:set-styles {:flex 1}
       :editor-tabs {:app-in-edit (w/gctx :params :app-in-edit)}}]}

    }

   :editor-tabs
   {:docker-layout {:layout {:type :row
                             :content [{:type :component
                                        :id :editor-pane
                                        :width 75
                                        :componentName "frame"
                                        :title "app"
                                        }
                                       {:type :component
                                        :id :properties-pane
                                        :componentName "frame"
                                        :title "properties"}]}
                    :frames {:editor-pane
                             {:app-in-edit-wrapper
                              {:app-in-edit (w/gctx :params :app-in-edit)}}
                             :properties-pane
                             {:widget-properties
                              {:widget-in-edit (w/gctx :scope :widget-in-edit)}}}}}

   :button {:dom-events {:click '(params :onclick)}
            :dom {:tag "vaadin-button"
                  :attrs {:theme '(params :theme)}
                  :text '(params :text)
                  :children '(if (params :icon)
                               [{:dom {:tag "iron-icon"
                                       :attrs {:icon (params :icon)}}}])}}

   :checkbox {:dom-events {:click '(params :onclick)}
              :dom {:tag "vaadin-checkbox"
                    :attrs {:checked '(params :checked)}}}

   :text-field {:dom-events {:input (w/gctx :params :oninput)}
                :dom {:tag "vaadin-text-field"
                      :attrs {:label (w/gctx :params :label)
                              :placeholder (w/gctx :params :placeholder)
                              :value (w/gctx :params :value)
                              }}}

   :v-layout {:set-styles {:display :flex
                           :flex-direction :column}
              :dom {:tag :div
                    :children '(params :children)}
              :meta {:part/name "Vertical layout"
                     :part/params {:children {:param/type :parts.params/children}}}
              }

   :h-layout {:set-styles {:display :flex
                           :flex-direction :row
                           :justify-content :space-between}
              :dom {:tag :div
                    :children '(params :children)}
              :meta {:part/name "Horizontal layout"
                     :part/params {:children {:param/type :parts.params/children}}}
              }

   :widget-properties
   {:set-styles {:height "100%"
                 :overflow :scroll}
    :local-state {:dialog false}
    :events-handler {:dialog dialog-open-event}
    :v-layout
    {:children [{:set-styles {:margin "16px 12px 0"}
                 :dom {:tag :h5 :text "widget"}}
                {:set-styles {:font-size "var(--lumo-font-size-m)"
                              :font-family "Fira Code"
                              :margin "0 12px"}
                 :dom {:tag :p
                       :text '(str (ctx :params :widget-in-edit)
                                   ;"  "
                                   ;(get (scope :ctx-of-widget-in-edit) ::w/instance-path)
                                   ;" " (hash (get (scope :ctx-of-widget-in-edit) ::w/instance-path))
                                   )}}
                {:set-styles {:margin "0 12px"
                              :font-family "Fira Code"
                              :font-size "var(--lumo-font-size-s)"
                              :color "var(--lumo-secondary-text-color)"
                              }
                 :dom {:tag :p
                       :text '(pr-str (get (scope :ctx-of-widget-in-edit) :params))
                       }}

                {;:if
                 :set-styles {:margin "0 12px"}
                 :button {:text "to parent"
                          :onclick {:event :select-widget
                                    ::w/instance-path '(butlast-without-nil (get (scope :ctx-of-widget-in-edit) ::w/instance-path))
                                    }}}

                #_{:set-styles {:font-size "var(--lumo-font-size-s)"
                                :font-family "Fira Code"}
                   :dom {:tag :p :text (expr '(str (get-in (ctx ::w/widgets) (ctx :params :widget-in-edit))))}}

                {:list-of {:items '(get-in (ctx ::w/widgets) (ctx :params :widget-in-edit))
                           :item-widget :editor.part-editor/part-properties
                           :param-for-key :part-id
                           :param-for-value :props
                           :common-params {:widget (w/gctx :params :widget-in-edit)}
                           }}

                {:set-styles {:margin "0 12px"}
                 :button {:text "Add part"
                          :onclick {:event :dialog :event/value true}}}


                {:vaadin-dialog
                 {:opened (w/gctx :scope :dialog)
                  :children [{:search-dialog {:items
                                              '(parts-for-search (ctx ::w/parts))
                                              #_(w/with-ctx #(incr/incr parts-for-search (get % ::w/parts)))}}]
                  :opened-changed {:event :dialog}}}
                ]}
    :order [:local-state :events-handler :set-styles :v-layout]
    }


   :app-in-edit-wrapper
   {:local-state {:edit-mode false}
    :dom-events {:click {:event :select-widget}}
    :dom {:tag :div
          :children [{:widget {:widget (w/gctx :params :app-in-edit)}}

                     #_{:search-dialog {:items (w/with-ctx #(incr/incr parts-for-search (get % ::w/parts)))}}

                     ]}}
   ;:order [:events-handler :dom-events]

   :dialog
   {:dom {:tag "vaadin-dialog"
          :attrs {:opened (w/gctx :params :opened)}
          :children [{:dom {:tag "template"
                            :children (w/gctx :params :children)}}]}}


   :search-dialog
   {:set-styles {:width 500}
    :local-state {:phrase ""
                  :active-item 0}
    :locals {:items (w/gctx :params :items)}
    :events-handler {:change-phrase (fn [event _] [[:set-local :phrase (:event/value event)]
                                                   [:set-local :active-item 0]])
                     :set-active (fn [event _] [:set-local :active-item (:item event)])
                     :keydown search-input-key-event}
    :v-layout
    {:children
     [{:dom-events {:keydown {:event :keydown}}
       :text-field {:placeholder "Search"
                    :oninput {:event :change-phrase}
                    }}
      {:list-of {:items (w/with-ctx #(incr/incr search-filter
                                                (get-in % [:scope :phrase])
                                                (get-in % [:scope :items])))
                 :item-widget :search-item
                 }}
      ]}}
   :search-item
   {:set-styles {:background '(if (= (ctx :scope :active-item)
                                     (ctx :params :key))
                                "var(--lumo-contrast-5pct)")
                 :border-radius "var(--lumo-border-radius)"
                 :padding "1px 10px"
                 :margin "5px 0"}
    :dom-events {:mouseenter {:event :set-active
                              :item '(ctx :params :key)}}
    :dom
    {:tag :div
     :children
     [{:set-styles {:margin-top "0.75rem"}
       :dom {:tag :h5 :text (w/gctx :params :item :name)}}
      {:set-styles {:font-size "var(--lumo-font-size-xxs)"
                    :line-height "var(--lumo-line-height-xs)"
                    :color "var(--lumo-secondary-text-color)"}
       :dom {:tag :p :text (w/gctx :params :item :desc)}}]}}

   })

(e/register-effect-handler!
  :set-local
  (fn [ctx field value]
    ;(js/console.log "FX" :set-local field value ctx)
    (reset! (get-in ctx [:scope field]) value)))