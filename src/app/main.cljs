(ns app.main
  (:require [core.incr :as incr]
            [mount.core :as mount :refer [defstate]]
            [core.engine :as e]
            [core.events :as events]
            core.styles
            [cljs.reader :as reader]
            [goog.net.XhrIo :as xhr]
    ;[clojure.browser.repl :as repl]
            clojure.string))

#_(defonce conn
         (repl/connect "http://localhost:9000/repl"))


(def log js/console.log)

(defn value [x]
  (if (implements? IDeref x) @x x))

(declare state-cell)

(events/register-effect-handler!
  :set-local
  (fn [ctx key val]
    (incr/cell-set! (get ctx key) val)))

(events/register-effect-handler!
  :set-parts-val
  (fn [_ widget part field val]
    (incr/cell-swap! state-cell
                     (fn [m]
                       (assoc-in m (concat [:parts] widget [part field]) val)))))

(events/register-effect-handler!
  :remove-part
  (fn [_ widget part]
    (incr/cell-swap! state-cell
                     (fn [m]
                       (update-in m (concat [:parts] widget) dissoc part)))))

(events/register-effect-handler!
  :add-part
  (fn [_ widget part]
    (incr/cell-swap!
      state-cell
      (fn [m]
        (assoc-in m
                  (concat [:parts] widget [part]) {})))))

(events/register-effect-handler!
  :add-part-child
  (fn [_ widget]
    (incr/cell-swap!
      state-cell
      (fn [m]
        (assoc-in m (concat [:parts] widget [:container :children (keyword (str "r" (rand-int 100)))])
                  {:w-p {:text "new"}})))))
(declare Expr)


(defn update-widget! [e]
  (let [parse-fn (case (:parse e)
                   :expr #(Expr. %)
                   :edn cljs.reader/read-string
                   :string str
                   str)]
    [:set-parts-val (:widget e) (:part e) (:field e) (parse-fn (:event/value e))]))

(defn remove-part-from-widget! [e]
  [:remove-part (:widget e) (:part e)])

(defn add-part-to-widget! [e ctx]
  [:add-part (:widget e) (keyword @(:part-name ctx))])

(defn mouseover-widget [e ctx]
  (let [{:strs [top left width height]} (:event/bounding-rect e)
        widget-path (:widget/path (@e/name->ctx (:event/target-id e)))]
    [[:set-local :frame-rect [top left width height]]
     [:set-local :mouseover-widget widget-path]]))

(defn widget-selected [e ctx]
  [:set-local :in-edit @(:mouseover-widget ctx)])

(defn add-child! [e _]
  [:add-part-child (:widget e)])

(defn set-input-val! [e ctx]
  [:set-local (:cell e) (:event/value e)])

(defn default-fields-editors [_ f]
  (get
    {:string :string-prop-editor
     :edn :edn-prop-editor}
    f f))

(defn icon-path [_ i]
  (str "/icons/" i ".svg"))

(defn register-tag-reader! [tag f]
  (swap! reader/*tag-table* assoc tag f))

(deftype Expr [js-text]
  e/IDisplayable
  (-display [this]
    {:expr-label {:text js-text
                  :app/merge-props true}})
  IFn
  (-invoke [this ctx]
    ((js/Function. "props" (str "return " js-text)) (clj->js (:props ctx))))

  IEquiv
  (-equiv [this other]
    (= js-text (.-js-text other)))

  IPrintWithWriter
  (-pr-writer [_ writer _]
    (write-all writer "#expr " (pr-str js-text))))

(register-tag-reader! "expr" (fn [text] (Expr. text)))

(deftype CtxGetter [path]
  e/IDisplayable
  (-display [this]
    {:w-p {:text path}})
  Fn
  IFn
  (-invoke [this ctx]
    (get-in ctx (->> path
                     (map (fn [p] (if (fn? p) (p ctx) p))))))
  Object
  (toString [this] (pr-str path))

  IEquiv
  (-equiv [this other]
    (= path (.-path other)))

  IPrintWithWriter
  (-pr-writer [_ writer _]
    (write-all writer "#ctx" (pr-str (vec path)))))

(register-tag-reader! "ctx" (fn [path] (CtxGetter. path)))

(defn gctx [& args]
  (CtxGetter. args))

(defn fn-ref-str [f]
  (pr-str (clojure.string/replace (.-name f) "$" ".")))

(deftype Calc [f args]
  Fn
  IFn
  (-invoke [this ctx]
    (apply f ctx (map (partial e/deep-deref ctx) args)))

  IEquiv
  (-equiv [this other]
    (and (= f (.-f other))
         (= args (.-args other))))

  IPrintWithWriter
  (-pr-writer [_ writer _]
    (write-all writer "#calc[" (fn-ref-str f) " " (pr-str args) "]")))

(defn resolve-fn-by-str [s]
  (js/eval s))

(register-tag-reader! "calc" (fn [[f args]] (Calc. (resolve-fn-by-str f) args)))

(defn calc [f & args]
  (Calc. f args))

(deftype EventHandler [f]
  Fn
  IFn
  (-invoke [this ctx]
    f)
  IEquiv
  (-equiv [this other]
    (= f (.-f other)))
  IPrintWithWriter
  (-pr-writer [_ writer _]
    (write-all writer "#event-handler " (fn-ref-str f))))

(register-tag-reader! "event-handler" (fn [f] (EventHandler. (resolve-fn-by-str f))))

(defn get-part [ctx p]
  (get-in ctx (cons :parts p)))

(defn get-part-field-value [ctx widget part field]
  #_(incr/fmap pr-str
               (get-in % (concat [:parts] @(get-in % [:props :widget]) [(get-in % [:props :part]) (get-in % [:props :field])])))
  (get-in ctx (concat [:parts] widget [part field])))

(defn get-part-field-value-str [ctx widget part field]
  #_(incr/fmap pr-str
               (get-in % (concat [:parts] @(get-in % [:props :widget]) [(get-in % [:props :part]) (get-in % [:props :field])])))
  @(get-in ctx (concat [:parts] widget [part field])))

(defn box-type-for-prop-value [ctx widget part field]
  ; thunk?
  (let [v @(get-in ctx (concat [:parts]
                               (value widget)
                               [part field]))]
    (cond
      (implements? e/IDisplayable v) v                      ;(e/-display v)
      (string? v) :string-prop-editor
      :else :edn-prop-editor
      )))

(defn load-module [name cb]
  (let [path (str "/data/" name "/ui.edn")
        receiver (fn [event]
                   (cb (cljs.reader/read-string (.getResponseText (.-target event)))))]
    ;; add ns to ::keywords?
    ;; load events, effects
    (xhr/send path receiver "GET")))

(defn load-module! []
  (load-module "test_app"
               (fn [parts]
                 (incr/cell-swap! state-cell (fn [m] (update m :parts merge parts)))))
  (incr/schedule-stabilization!))

(def app
  {:app-preview {:w-p {:text "preview"}
                 :set-styles {:color "#888"}
                 :order [:set-styles :w-p]}

   :test-items {:list-of {:items {0 0 1 1 2 42}
                          :item-widget :test-list-item}}
   :test-list-item {:w-p {:text (Expr. "'a' + props.val") #_(str "a" (get-in % [:props :val]))}}
   :test-list-item2 {:w-p {:text (gctx :props :key) #_(Expr. "'b' + props.val")}}

   :w1 {:set-styles {:display :grid
                     :gridTemplateColumns "1fr 300px"
                     :gridTemplateRows "40px 1fr"
                     :height "100vh"}
        :local-state {;:in-edit [:app-preview]
                      :in-edit [:test-list-item]
                      :frame-rect [0 0 0 0]
                      :mouseover-widget []}
        :container
        {:children {:top-bar {:set-styles {:grid-column "1/3"
                                           :grid-row "1/2"}
                              :top-bar {}}
                    :left {:set-styles {;:background "#e8e8e8"
                                        ;:padding 15
                                        }
                           :container {:children {:mod {:load-module {}}}}}
                    :right {:widget-editor {:in-edit (gctx :in-edit)}}
                    :frame :hover-frame
                    }}
        :events-handler {:mouseover-widget (EventHandler. mouseover-widget)
                         :widget-selected (EventHandler. widget-selected)}
        :order [:local-state :set-styles :events-handler :container]
        }

   :app' {:container {:children {:r0 :app-preview :r1 :test-items}}
          :dom-events {:mouseover {:event :mouseover-widget}
                       :click {:event :widget-selected}}
          :order [:dom-events :container]}

   :hover-frame {:set-styles {:position :absolute
                              :outline "1px solid #00e"
                              :top (gctx :frame-rect 0)
                              :left (gctx :frame-rect 1)
                              :width (gctx :frame-rect 2)
                              :height (gctx :frame-rect 3)
                              :pointer-events "none"
                              }
                 :dom {:tag :div}}

   :separator {:set-styles {:border-width "1px 0 0 0"
                            :border-style "solid"
                            :border-color "#cec9c4"}
               :dom {:tag :hr}}

   :top-bar {:set-styles {:background "#f7f7f5"
                          :border-bottom "1px solid #e0e0e0"
                          :display :flex
                          :align-items :center}
             :container {:children [{:icon {:icon "menu"}
                                     :set-styles {:margin 10
                                                  :opacity 0.3}}
                                    {:icon {:icon "undo"}
                                     :set-styles {:margin 10
                                                  :opacity 0.3}}
                                    {:icon {:icon "redo"}
                                     :set-styles {:margin 10
                                                  :opacity 0.3}}
                                    {:dom {:tag :div}
                                     :set-styles {:flex-grow 1}}
                                    #_{:dom {:tag :div
                                             :text "Action / find ..."}
                                       :set-styles {:flex-grow 1
                                                    :color "#999"
                                                    :font-size 13
                                                    :padding-bottom 5
                                                    :text-transform :uppercase
                                                    :border-bottom "1px solid #ccc"}}
                                    #_{:dom {:tag :div}
                                       :set-styles {:flex-grow 1}}
                                    {:icon {:icon "database"}
                                     :set-styles {:margin 10
                                                  :opacity 0.3}}
                                    {:icon {:icon "directions-fork"}
                                     :set-styles {:margin 10
                                                  :opacity 0.3}}
                                    ]}
             }

   :widget-editor {;:locals {:in-edit #(deref (get-in % [:props :in-edit]))}
                   :container
                   {:children
                    [{:w-p {:text (gctx :props :in-edit)}}
                     {:add-part-to-widget {:widget (gctx :props :in-edit) #_(deref (get-in % [:props :in-edit]))}}

                     {:list-of {:items (calc get-part (gctx :props :in-edit))
                                #_(gctx :parts (gctx :props :in-edit))
                                #_(get-in % (cons :parts @(get-in % [:props :in-edit])))
                                :common-props {:widget (gctx :props :in-edit) #_(deref (get-in % [:props :in-edit]))}
                                :key-field-name :part
                                :val-field-name :part-props
                                :item-widget :part-editor-wrapper}}]}
                   :events-handler {:update-widget (EventHandler. update-widget!)
                                    :remove-part-from-widget (EventHandler. remove-part-from-widget!)
                                    :add-child (EventHandler. add-child!)}
                   :set-styles {:background-color "#f7f7f5"
                                :border-left "1px solid #e0e0e0"}
                   :order [:set-styles :events-handler :container]
                   }

   :add-part-to-widget {:local-state {:part-name ""}
                        :events-handler {:add-part (EventHandler. add-part-to-widget!)}
                        :container
                        {:children
                         [{:input {:bind-cell :part-name}}
                          {:button {:text "add part"
                                    :click {:event :add-part
                                            :widget (gctx :props :widget)}}}]}}

   :part-editor-wrapper {:container
                         {:children
                          [{:link {:text "x"
                                   :click {:event :remove-part-from-widget
                                           :widget (gctx :props :widget)
                                           :part (gctx :props :part)}}
                            :set-styles {:float :right
                                         :color "#666"
                                         :font-size 12}}
                           {:w-p {:text ["\u2630 " (gctx :parts (gctx :props :part) :meta :name)]
                                  #_[(gctx :parts (gctx :props :part) :meta :name) " (" (gctx :props :part) ")"]}
                            :set-styles {:margin-top 0
                                         :color "#888"
                                         :text-transform "uppercase"
                                         :font-size 12
                                         }}
                           {:default-part-editor {:widget (gctx :props :widget)
                                                  :part (gctx :props :part)}}
                           ]}
                         :set-styles {:padding 10
                                      :border-bottom "1px solid #e0e0e0"}
                         }

   :default-part-editor {:list-of {:item-widget :part-prop-editor
                                   :items (gctx :parts (gctx :props :part) :meta :props) #_(get-in % [:parts (get-in % [:props :part]) :meta :props])
                                   :key-field-name :field
                                   :val-field-name :field-data
                                   :common-props {:widget (gctx :props :widget)
                                                  :part (gctx :props :part)}}}

   :part-prop-editor {:container
                      {:children
                       [{:w-p {:text (gctx :props :field-data :label)}
                         :set-styles {:font-size 13
                                      :margin-bottom 5
                                      :color "#666"}}
                        {:call
                         {:widget (calc box-type-for-prop-value
                                        (gctx :props :widget)
                                        (gctx :props :part)
                                        (gctx :props :field))
                          ;(calc default-fields-editors (gctx :props :field-data :type))
                          :props
                          {:widget (gctx :props :widget)
                           :part (gctx :props :part)
                           :field (gctx :props :field)}}}]}}

   :log-params {:w-p {:text (gctx :props)}}

   :string-prop-editor {:input {:value (calc get-part-field-value-str
                                             (gctx :props :widget)
                                             (gctx :props :part)
                                             (gctx :props :field))}
                        :set-styles {:width "100%"
                                     :background "none"
                                     :border-width "0 0 1px 0"
                                     :border-color "rgba(0,0,0,.25)"}
                        :dom-events {:keyup {:event :update-widget
                                             :parse :string
                                             :widget (gctx :props :widget)
                                             :part (gctx :props :part)
                                             :field (gctx :props :field)}}}
   :edn-prop-editor {:textarea {:value (calc get-part-field-value-str
                                             (gctx :props :widget)
                                             (gctx :props :part)
                                             (gctx :props :field))}
                     :set-styles {:width "100%"}
                     :dom-events {:keyup {:event :update-widget
                                          :parse :edn
                                          :widget (gctx :props :widget)
                                          :part (gctx :props :part)
                                          :field (gctx :props :field)}}}
   :children-prop-editor {:container
                          {:children
                           [{:list-of {:item-widget {:w-p {:text [(gctx :props :key) "=>" (gctx :props :val)]}}
                                       :items (calc get-part-field-value
                                                    (gctx :props :widget)
                                                    (gctx :props :part)
                                                    (gctx :props :field))}}
                            {:button {:text "add child"
                                      :click {:event :add-child
                                              :widget (gctx :props :widget)}}}]}}

   :expr-label {:container {:children {:i {:input {:value (gctx :props :text)}
                                            :set-styles {:background "none"
                                                         :color "#fff"
                                                         :border "none"}
                                            :dom-events {:keyup {:event :update-widget
                                                                 :parse :expr
                                                                 :widget (gctx :props :widget)
                                                                 :part (gctx :props :part)
                                                                 :field (gctx :props :field)}}}}}
                 :set-styles {:background "#A26248"
                              :display :inline-block
                              :padding "5px 8px"
                              :border-radius 3
                              :font-size 13
                              :font-family "Fira Code"
                              :color "#fff"
                              :box-shadow "0 2px 6px 0 rgba(0,0,0,0.2)"
                              :width "100%"
                              ":before" {:content "\"=>\""
                                         :margin-right 8
                                         :padding-right 8
                                         :border-right "1px solid #ccc"}}}

   :icon {:dom {:tag :img
                :attrs {:src (calc icon-path (gctx :props :icon))}}
          :meta {:priority 0}}

   })

(def state-0
  (merge-with
    merge
    core.events/app-cfg
    core.styles/app-cfg
    {:parts
     {:w-p {:intercept {:after (fn w-p [ctx params]
                                 ;:(println "w-p" params)
                                 (if (vector? (:text params))
                                   (assoc ctx
                                     :dom {:tag :p
                                           :children (->> (:text params)
                                                          (map-indexed
                                                            (fn [i s] [i {:tag :span :text (e/deref-one ctx s)}]))
                                                          (into {}))})
                                   (assoc ctx
                                     :dom {:tag :p
                                           :text (:text params)})))}
            :meta {:name "Text paragraph"
                   :doc ""
                   :props {:text {:label "Text"
                                  :doc ""
                                  :type :string}}
                   :priority 0
                   ;:part-editor  :default-part-editor
                   }}
      :input {:intercept {:after (fn input-after [ctx params]
                                   ;(println "input" params (:props ctx))
                                   (if-let [bind (:bind-cell params)]
                                     (let [cell (get ctx bind)]
                                       (assert (implements? incr/ICell cell))
                                       (-> ctx
                                           (assoc :dom {:tag (or (:tag params) :input)
                                                        :attrs {:placeholder (:placeholder params)}
                                                        :value cell})
                                           (core.events/add-handlers {:set-input-val set-input-val!})
                                           (core.events/add-events {:keyup {:event :set-input-val
                                                                            :cell bind}})
                                           ))
                                     (assoc ctx
                                       :dom {:tag (or (:tag params) :input)
                                             :attrs {:placeholder (:placeholder params)}
                                             :value (:value params)})))}
              :meta {:priority 0}}
      :textarea {:pass {:widget :input
                        :props {:tag :textarea}
                        :pass-props true}
                 :meta {:priority 0}}

      :button {:intercept {:after (fn [ctx params]
                                    (-> ctx
                                        (assoc :dom {:tag :button
                                                     :text (:text params)})
                                        (core.events/add-events {:click (:click params)})))}
               :meta {:priority 0
                      :props {:text {:label "Text" :type :string}
                              :click {:label "On click" :type :string}}}}

      :link {:intercept {:after (fn [ctx params]
                                  (-> ctx
                                      (assoc :dom {:tag :a
                                                   :text (:text params)})
                                      (core.events/add-events {:click (:click params)})))}
             :meta {:priority 0}}

      :dom {:intercept {:after (fn [ctx params]
                                 (assoc ctx
                                   :dom params))}
            :meta {:priority 0}}
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
                                               #_(container-children (:children params))}))}
                  :meta {:name "Container"
                         :props {:children {:label "Children list"
                                            :type :children-prop-editor}}
                         :priority 0}}


      :list-of {:intercept {:after (fn [ctx params]
                                     (let [items (e/deref-one ctx (:items params))
                                           items (if (sequential? items)
                                                   (zipmap (range) items)
                                                   items)]
                                       ;(println "L" items params ctx)
                                       (when (map? items)
                                         (assoc ctx
                                           :dom {:tag :div
                                                 :children (fn [ctx]
                                                             (->> (e/deref-one ctx (:items params)) ; sic!
                                                                  (map
                                                                    (fn [[k v]]
                                                                      [k (e/render-child k #_(-> ctx
                                                                                                 (assoc :props (:common-props params))
                                                                                                 (assoc-in [:props (get params :key-field-name :key)] k)
                                                                                                 (assoc-in [:props (get params :val-field-name :val)] (get (:items params) k)))
                                                                                         ctx
                                                                                         (:item-widget params)
                                                                                         (assoc (:common-props params)
                                                                                           (get params :key-field-name :key) k
                                                                                           (get params :val-field-name :val) (get (:items params) k)) ; sic!
                                                                                         [:list-of :item-widget])]))
                                                                  (into {})))}))))}
                :meta {:name "List of"
                       :props {:item-widget {:label "Item widget"
                                             :type :string}
                               :items {:label "Items"
                                       :type :string}}
                       :priority 0}}
      :call {:intercept {:after (fn [ctx params]
                                  #_(println "CALL " params
                                             (e/deref-one ctx (:widget params)))
                                  (assoc ctx :dom
                                             (e/render-child "call" ctx (e/deref-one ctx (:widget params)) (:props params) [:call :widget])))}
             :meta {:priority 0}}

      :pass {:intercept {:before (fn pass-before [ctx params]
                                   (update ctx ::e/queue into
                                           (e/get-interceptors ctx
                                                               (e/deref-one ctx (:widget params))
                                                               (if (:pass-props params)
                                                                 (merge (:props ctx) (:props params))
                                                                 (:props params)))))
                         ;:after
                         #_(fn pass-after [ctx _]
                             (js/console.log "PASS After" (::e/queue ctx) (::e/stack ctx))
                             ctx)}
             :meta {:priority 0}}

      :pass-overwrite {:intercept {:before (fn [ctx params]
                                             (update ctx ::e/queue into
                                                     (e/get-interceptors ctx
                                                                         (merge-with merge
                                                                                     (e/get-part-as-map ctx
                                                                                                        (e/deref-one ctx (:widget params)))
                                                                                     (:overwrites params))
                                                                         (:props params))))}
                       :meta {:priority 0}}


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

      :locals {:intercept {:before (fn [ctx params]
                                     ;(println "Locals" params)
                                     (reduce (fn [ctx [key val]]
                                               (assoc-in ctx [:locals key] val))
                                             ctx
                                             params))}}
      :load-module {:intercept {:before (fn [ctx params]
                                          #_(-> ctx
                                                (core.events/add-handlers {:load-module (EventHandler. load-module!)}))
                                          (load-module!)
                                          (assoc ctx :dom
                                                     (e/render-child nil #_"submod" ctx :test_app nil [:load-module]))
                                          )}
                    :meta {:priority 0}}

      }}
    {:parts app}))

(def state-cell (incr/cell state-0))

(def ctx-0
  (with-meta
    {:parts (get state-cell :parts)}
    {:incr/name "N"}))

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
