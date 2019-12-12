(ns editor.expr-blocks
  (:require [clojure.zip :as zip]
            [runtime.widgets :as w]
            [runtime.expr :as e]
            [incr.core :as incr]
            cljs.reader
            [parts.params :as params]
            [com.rpl.specter :as specter]
            [malli.core :as m]))

(defn expr-zipper [expr]
  (zip/zipper
    (fn [x]
      (or (list? x) (vector? x) (map? x)))
    seq
    (fn [node children]
      (cond
        (vector? node) (with-meta (vec children) (meta node))
        (map? node) (with-meta (into {} (map (fn [[k v]] [k v]) children))
                               (meta node))
        (or (list? node) (seq? node)) (with-meta (apply list children) (meta node))
        ))
    expr))

(defn block [& {:keys [label children expr-zip inline]}]
  {:block
   {:label label
    :children children
    :expr-zip expr-zip
    :inline inline
    }})

(defn choice [& {:keys [expr-zip options placeholder]}]
  {:block-choice {:expr-zip expr-zip
                  :placeholder (or placeholder "Choose")
                  :options options}})

(defn fns-list [ctx]
  [{:text "fn: if"
    :value '(if)}

   {:text "params"
    :value '(params)}

   {:text "scope"
    :value '(scope)}

   ])

(declare input-styles block-or-input2)

(defn string-block [expr-zip spec ctx]
  (block :label "str"
         :expr-zip expr-zip
         :children
         [{:dom-events {:input {:event :input-changed
                                ;:event-subscription/stop-propagation true
                                }}
           :set-styles input-styles
           :dom {:tag :input
                 :attrs {:value (zip/node expr-zip)}
                 }}]
         ))

(declare fn-call getter-block)

(defn children-zippers [zip]
  (take-while some? (iterate zip/right (zip/down zip))))

(defn append-child-zipper [zip]
  (-> zip
      (zip/append-child nil)
      (zip/down)
      (zip/rightmost)))

(defn getter-block [expr-zip spec ctx]
  ;(js/console.log "zip" expr-zip)
  (let [[k & path] (zip/node expr-zip)
        k (keyword k)
        ;arg-zips (drop 1 (take-while some? (iterate zip/right (zip/down expr-zip))))
        next-arg (append-child-zipper expr-zip)

        dict (incr/incr get
                        (get-in ctx [:scope :ctx-of-widget-in-edit])
                        k)
        val (incr/value @(incr/incr get-in dict path))
        next-keys (when (map? val) (keys val))

        path (map name path)
        ]

    ;(js/console.log "getter-block" k @dict val path next-keys (incr/value (get-in ctx [:scope :ctx-of-widget-in-edit])))

    (block
      :label (name k)
      :children
      (concat
        [{:set-styles {:display :inline
                       :font-family "Fira Code"
                       :font-size "var(--lumo-font-size-s)"
                       :font-weight 500
                       :line-height "30px"}
          :dom {:tag :div :text (clojure.string/join "->" (butlast path))}}]

        (when path
          [(block
             :expr-zip (-> expr-zip (zip/down) (zip/rightmost))
             :children [{:set-styles {:font-family "Fira Code"
                                      :font-size "var(--lumo-font-size-m)"
                                      :font-weight 500
                                      :line-height "30px"}
                         :dom {:tag :div :text (str (when (butlast path) "->") (last path))}}]
             :inline true)])

        (when next-keys
          [(choice
             :expr-zip next-arg
             :options
             (fn [phrase]
               (map (fn [x]
                      {:text (str x) :value x})
                    next-keys)))])
        ))))

(def fn-blocks
  {'scope getter-block
   'params getter-block
   'handler getter-block
   'event getter-block
   })

(defn fn-call [expr-zip spec ctx]
  (let [f (first (zip/node expr-zip))]
    (if-let [b (fn-blocks f)]
      (b expr-zip spec ctx)
      (let [arg-zips (drop 1 (children-zippers expr-zip))
            next-arg (append-child-zipper expr-zip)]
        (block
          :label "fn"
          :children
          (concat
            [{:dom {:tag :div :text (str f)}}]
            (mapv #(block-or-input2 % spec ctx) arg-zips)
            [(block-or-input2 next-arg spec ctx)]
            ))))))

(defn boolean-input [expr-zip spec ctx]
  (block :label "bool"
         :expr-zip expr-zip
         :children
         [{:dom-events {:change {:event :input-changed}}
           :dom {:tag "vaadin-checkbox"
                 :attrs {:checked (zip/node expr-zip)}
                 }}]
         ))

(declare map-block params-block)

(defn anonymous-widget [expr-zip spec ctx]
  (let [;parts (map key (zip/node expr-zip))
        ]
    (block
      :label "w"
      :expr-zip expr-zip
      :children [{:h-layout
                  {:children
                   [{:set-styles {:font-size "var(--lumo-font-size-l)"
                                  :font-weight 500
                                  :align-self :center
                                  :color "var(--lumo-secondary-text-color)"}
                     :dom {:tag :div :text "Anonymous widget"}}
                    {:set-styles {:margin "-1px 0"}
                     :button {:text "edit"
                              :theme :tertiary
                              :onclick {:event :select-widget
                                        ::w/instance-path (conj (-> ctx :scope :ctx-of-widget-in-edit incr/value ::w/instance-path) (count (zip/lefts expr-zip)))} ;fixme?
                              }}]}}
                 #_{:dom {:tag :div :text (pr-str parts)}}
                 ])))

(defn widget-call-block [expr-zip spec ctx]
  ;(js/console.log "widget-call-block" expr-zip spec)
  (let [val (zip/node expr-zip)
        {:keys [widget params]} val
        widget-def (get @(::w/widgets ctx) widget)
        params-spec (some-> widget-def :meta :part/params)
        params-zip (-> expr-zip
                       (zip/down) (zip/right) (zip/down) (zip/right) ;fixme
                       )
        params-zip (if (empty? params)
                     (let [widget-params (w/infer-params-of-widget widget-def)]
                       (-> params-zip (zip/replace (zipmap widget-params (repeat nil)))))
                     params-zip)]
    (block
      :label "w"
      :expr-zip expr-zip
      :children [{:h-layout
                  {:children
                   [{:set-styles {:font-size "var(--lumo-font-size-l)"
                                  :font-weight 500
                                  :align-self :center}
                     :dom {:tag :div :text (or (some-> widget-def :meta :part/name) (pr-str widget))}}
                    {:set-styles {:margin "-1px 0"}
                     :button {:text "edit"
                              :theme :tertiary
                              :onclick {:event :select-widget
                                        ::w/instance-path (seq (conj (-> ctx :scope :ctx-of-widget-in-edit incr/value ::w/instance-path)
                                                                     (count (zip/lefts expr-zip))))}}}]}}
                 (params-block params-zip params-spec ctx :with-other-params true)
                 ]
      )))

(defn part-call-block [expr-zip spec ctx]
  ;(js/console.log "part-call-block" expr-zip)
  (let [val (zip/node expr-zip)
        [part-id params] val
        part-def (get (::w/parts ctx) part-id)
        params-spec (some-> part-def :part/params)
        params-zip (-> expr-zip
                       (zip/down) (zip/right))]
    (block
      :label "p"
      :expr-zip expr-zip
      :children [{:h-layout
                  {:children
                   [{:set-styles {:font-size "var(--lumo-font-size-l)"
                                  :font-weight 500
                                  :align-self :center}
                     :dom {:tag :div :text (or (some-> part-def :part/name) (pr-str part-id))}}
                    {:set-styles {:margin "-1px 0"}
                     :button {:text "edit"
                              :theme :tertiary
                              :onclick {:event :select-widget
                                        ::w/instance-path (seq (conj (-> ctx :scope :ctx-of-widget-in-edit incr/value ::w/instance-path) (count (zip/lefts expr-zip))))}}}]}}
                 (params-block params-zip params-spec ctx #_:with-other-params #_true)
                 ])))


(defn child-block [expr-zip spec ctx]
  ;(js/console.log "child-block" expr-zip)
  (let [val (zip/node expr-zip)]
    (if (< 1 (count val))
      (anonymous-widget expr-zip spec ctx)
      (if (= :widget (ffirst val))
        (widget-call-block (-> expr-zip (zip/down) (zip/down) (zip/right))
                           nil
                           ctx)
        (part-call-block (-> expr-zip (zip/down))
                         nil
                         ctx)
        ))))

(declare block-or-input2)


(defn array-block [expr-zip spec ctx]
  (let [expr (zip/node expr-zip)
        [expr expr-zip] (if (nil? expr)
                          [[] (expr-zipper [])]
                          [expr expr-zip])
        zippers (children-zippers expr-zip)
        new-child-zip (append-child-zipper expr-zip)
        ]
    (block
      :label "[]"
      :children (concat
                  (map #(block-or-input2 % nil #_::params/child ctx) zippers)
                  #_[(choice
                     new-child-zip
                     (fn [phrase]
                       (map
                         (fn [[k v]]
                           {:text k :value {:widget {:widget k
                                                     :params (zipmap (w/infer-params-of-widget (get @(:runtime.widgets/widgets ctx) k))
                                                                     (repeat nil))}}})
                         @(:runtime.widgets/widgets ctx))))]))))

(defn default-values [params]
  (if (vector? params)
    (->> (rest params)
         (keep
           (fn [[k desc spec]]
             (when-let [v (when (map? desc) (:param/default desc))]
               [k (if (implements? IWithMeta v)
                    (with-meta v {:default-value? true})
                    v)])))
         (into {}))
    {}))

(defn new-widgets-list [ctx]
  (concat
   (map
     (fn [[k v]]
       (let [widget-def (get @(:runtime.widgets/widgets ctx) k)]
         {:label (str "w: " k)
          :value {:widget {:widget k
                           :params
                           (merge
                             (zipmap (w/infer-params-of-widget widget-def)
                                     (repeat nil))
                             (default-values (some-> widget-def :meta :part/params)))}}}))
     @(:runtime.widgets/widgets ctx))

   (keep
     (fn [[k v]]
       (when (:part/render v)
         {:label (str "p: " (or (:part/name v) k))
          :value {k (default-values (:part/params v))}}))
     (:runtime.widgets/parts ctx))))

(defn children-block [expr-zip spec ctx]
  (let [expr (zip/node expr-zip)
        [expr expr-zip] (if (nil? expr)
                          [[] (expr-zipper [])]
                          [expr expr-zip])
        zippers (children-zippers expr-zip)
        new-child-zip (append-child-zipper expr-zip)
        ]
    (block
      :label "[]"
      :children (concat
                  (map #(block-or-input2 % ::params/child ctx) zippers)
                  [(block-or-input2 new-child-zip ::params/child ctx)
                   #_(choice
                     :expr-zip new-child-zip
                     :placeholder "new child..."
                     :options
                     (fn [phrase]
                       (new-widgets-list ctx)))]))))

(declare block-or-input2)

(defn map-entry-block [expr-zip properties spec ctx]
  ;(js/console.log "map-entry block" expr-zip properties spec)
  (let [[k v] (zip/node expr-zip)
        val-zip (last (children-zippers expr-zip))]
    (block
      :expr-zip expr-zip
      :children
      [{:set-styles {:margin 0}
        :editor.part-editor/field {:label (or (:param/name properties) (name k))
                                   :field (block-or-input2 val-zip spec ctx)}}
       ])))

(defn spec-name [spec]
  (if (vector? spec)
    (first spec)
    spec))

(defn spec-form-name [spec]
  (spec-name (params/form spec)))


(defn keys-options [phrase]
  [{:text (str ":" phrase) :value [(keyword phrase) nil]}])

(defn map-block [expr-zip spec ctx & {:keys [without-label]}]
  ;(js/console.log "map block" expr-zip)
  (let [expr (zip/node expr-zip)
        [expr expr-zip] (if (nil? expr)
                          [{} (expr-zipper {})]
                          [expr expr-zip])
        zippers (children-zippers expr-zip)
        new-child-zip (-> expr-zip
                          (zip/append-child [nil nil])
                          (zip/down)
                          (zip/rightmost)) ; ??
        #_(append-child-zipper expr-zip)
        map-entry-spec (fn [k]
                         (if spec
                           (case (spec-form-name spec)
                             :map (->> (rest (params/form spec))
                                       (filter #(= k (first %)))
                                       first
                                       last)
                             :map-of (or (some-> spec params/form last
                                                 any?))
                             (do (js/console.warn "No matching map-entry-spec" spec)
                                 any?)
                             )
                           any?))]
    (block
      :label (when-not without-label "{}")
      :expr-zip expr-zip
      :children (concat
                  (map #(map-entry-block %
                                         nil                ;; fixme
                                         (map-entry-spec (first (zip/node %)))
                                         ctx)
                       zippers)
                  [(choice
                     :expr-zip new-child-zip
                     :placeholder "new map entry..."
                     :options keys-options)]))))

(defn params-block [expr-zip spec ctx & {:keys [with-other-params]}]
  ;(js/console.log "params-block" expr-zip spec)
  (let [expr (zip/node expr-zip)
        [expr expr-zip] (if (nil? expr)
                          [{} (expr-zipper {})]
                          [expr expr-zip])
        zippers (->> (children-zippers expr-zip)
                     (map (fn [z] [(first (zip/node z)) z]))
                     (into {}))
        new-child-zip (fn [k]
                        (-> expr-zip
                            (zip/append-child [k nil])
                            (zip/down)
                            (zip/rightmost)))
        zipper (fn [k]
                 (or (zippers k)
                     (new-child-zip k)))

        children-spec (rest spec)
        spec-keys (set (map first children-spec))

        ]
    (block
      :expr-zip expr-zip
      :children (concat
                  (map (fn [[k properties spec]]
                         (map-entry-block (zipper k)
                                          ;; optional params
                                          (when spec properties)
                                          (or spec properties)
                                          ctx))
                       (concat
                         children-spec
                         (when with-other-params
                           (keep
                             (fn [k] (when-not (spec-keys k)
                                       [k {} any?]))
                             (keys expr)))
                         )
                       (merge
                         (when with-other-params
                           (zipmap (keys expr) (repeat nil)))
                         children-spec))
                  (when with-other-params
                    [(choice
                       :expr-zip new-child-zip
                       :placeholder "new param..."
                       :options keys-options)])))))


(def type->block-registry (atom {}))

(defn register-block! [spec f]
  (swap! type->block-registry assoc spec f))

(defn string-or-keyword? [x]
  (or (string? x) (keyword? x)))

(defn contains-substr? [str sub]
  (or (nil? sub) (and str (not= -1 (.indexOf str sub)))))

(def value-providers (atom {}))

(defn register-value-provider [types f]
  (doseq [t types]
    (swap! value-providers update t conj f)))

(defn string-provider [spec ctx]
  (fn [phrase]
    [{:label (str "\"" phrase "\"") :value phrase}]))

(register-value-provider [string?] string-provider)

(defn child-provider [spec ctx]
  (let [widgets (new-widgets-list ctx)]
    (fn [phrase]
      (filter
        #(contains-substr? (:label %) phrase)
        widgets))))

(register-value-provider [::params/child] child-provider)

(register-value-provider [::params/children]
                         (fn [spec ctx]
                           (let [child-p (child-provider spec ctx)]
                             (fn [phrase]
                               (map
                                 #(update % :value vector)
                                 (child-p phrase))))))

(def functions-registry
  (atom {'scope {:doc "Get value from scope"}

         }))

(defn function-provider [spec ctx]
  (fn [phrase]
   (when (and phrase (pos? (.-length phrase)))
     (let [fns (filter #(contains-substr? (name (key %)) phrase)
                       @functions-registry)]
       (map
         (fn [[k v]]
           {:label (str k ": " (:doc v)) :value (list k)}
           )
         fns)))))

(defn gen-provider [spec ctx & extra-providers]
  (let [providers (get @value-providers (spec-name spec) [])
        providers (concat providers extra-providers)
        ps (map #(% spec ctx) providers)]
    (fn [phrase]
      (mapcat
        #(% phrase)
        ps))))

(defn map-entry-provider [spec ctx]
  (case (spec-name spec)
    :map (fn [phrase]
           (->> (rest spec)
                (map first)
                (filter #(contains-substr? (name %) phrase))
                (map #(do {:label (str "key: " %) :value [% nil]}))))
    :map-of (let [p (gen-provider (second spec) ctx)]
              (fn [phrase]
                (->> (p phrase)
                     (map (fn [m] (-> m
                                      (update :label #(str "kv: " %))
                                      (update :value #(do [% nil]))))))))))

(defn map-provider [spec ctx]
  (fn [phrase]
   (map
     #(update % :value (fn [[k v]] {k v}))
     ((map-entry-provider spec ctx) phrase))))

(register-value-provider [:map :map-of] map-provider)

(register-value-provider [:enum]
                         (fn [spec ctx]
                           (let [options (rest spec)]
                             (fn [phrase]
                               (->> options
                                    (filter #(contains-substr? (str %) phrase))
                                    (map #(do {:label (str %) :value %})))))))

(defn choice-for-input [expr-zip spec ctx]
  (choice
    :placeholder "Choose"
    :expr-zip expr-zip
    :options (incr/incr gen-provider spec ctx
                        function-provider)
    #_(fn [phrase]
        (mapcat
          #(% phrase spec ctx)
          providers)

        #_(concat
            [{:text (str "\"" phrase "\"") :value phrase}
             (if-let [v (try (cljs.reader/read-string phrase)
                             (catch js/Error _ nil))]
               {:text (str "edn: " phrase) :value v})]
            (fns-list ctx)
            ))
    )
  )

(defn expr-input [expr-zip spec ctx]
  ;(js/console.log "expr-input" expr-zip )
  (let [expr (zip/node expr-zip)]
    (cond
      (string? expr) (string-block expr-zip spec ctx)
      (number? expr) (string-block expr-zip spec ctx)
      (keyword? expr) (string-block expr-zip spec ctx)
      (list? expr) (fn-call expr-zip spec ctx)
      (vector? expr) (array-block expr-zip spec ctx)
      (map? expr) (map-block expr-zip spec ctx)

      (nil? expr) (choice-for-input expr-zip spec ctx)

      :else ;; xxx
      (string-block expr-zip spec ctx)

      )))


(register-block! ::params/tag-name string-block)
(register-block! ::params/string string-block)
(register-block! ::params/children children-block)
(register-block! ::params/child child-block)
(register-block! ::params/dom-attrs map-block)
(register-block! ::params/locals map-block)
(register-block! ::params/widget string-block)
(register-block! ::params/params params-block)
(register-block! ::params/widget-call widget-call-block)

(register-block! :map map-block)


(defn type->block [spec]
  (get @type->block-registry (spec-name spec)))

(defn get-block-type [spec value]
  (if (or (nil? value)
          (list? value) ;; expr
          )
    expr-input

    (if-let [block (type->block spec)]
      block
      ;string-block ;; xxx

      expr-input
      #_(cond
        (string? value) string-block
        (number? value) string-block
        (keyword? value) string-block
        (map? value) map-block
        :else string-block
        )

      )))

(defn block-or-input2 [expr-zip spec ctx]
  (when-let [block (get-block-type spec (zip/node expr-zip))]
    (incr/incr block expr-zip spec ctx)))

(defn expr-field [expr spec ctx]
  ;(js/console.log "expr-field" expr spec)
  (let [expr-zip (expr-zipper expr)]
    (block-or-input2 expr-zip (or spec [:map-of keyword? any?]) ctx)))


(extend-protocol IStack
  LazySeq
  (-peek [coll]
    ;(js/console.log "-peek" coll)
    (first coll))
  (-pop [coll] (rest coll)))

(defn select-handler [event ctx]
  (let [zipper (incr/value (get-in ctx [:scope :expr-zip]))]
    ;(js/console.log "select-handler" zipper (:value event) ctx)
    [[:emit-event {:event :expr-changed
                   :value (-> zipper
                              (zip/replace
                                (:value event))
                              (zip/root))
                   :event/elem-call-id (:event/elem-call-id event) ;fixme
                   }]]))


(defn on-block-keypress [event ctx]
  (case (:event/key-pressed event)
    "Backspace" [[:emit-event {:event :expr-changed
                               :value (let [[_ path :as zipper] (incr/value (get-in ctx [:scope :expr-zip]))]
                                        ;(js/console.log "Backspace" zipper)

                                        (if (nil? path)
                                          nil
                                          (-> zipper
                                              (zip/remove)
                                              (zip/root))))
                               :event/elem-call-id (:event/elem-call-id event) ;fixme
                               }]]
    nil))

(defn input-changed [event ctx]
  (let [zipper (incr/value (get-in ctx [:scope :expr-zip]))]
    [[:emit-event {:event :expr-changed
                   :value (-> zipper
                              (zip/replace
                                (:event/value event))
                              (zip/root))
                   :event/elem-call-id (:event/elem-call-id event) ;fixme
                   }]]))

(def input-styles
  {:border-radius "var(--lumo-border-radius)"               ; "0 var(--lumo-border-radius) var(--lumo-border-radius) 0"
   :background "var(--lumo-contrast-10pct)"
   :padding "0 6px"
   :font-weight 500
   :border 0
   :font-size "var(--lumo-font-size-m)"
   :height 30
   :width "100%"
   })

(def widgets
  {
   :block
   {:set-styles '(if (params :label)
                   {:background "rgba(22, 118, 243, 0.02)" ;"rgba(24, 39, 57, 0.05)" ;"var(--lumo-primary-color-10pct)"
                    :border "1px solid var(--lumo-contrast-10pct)" ;var(--lumo-primary-color)
                    :border-radius "var(--lumo-border-radius)"
                    ;:display (if (params :inline) :inline-flex :flex)
                    :display :flex
                    ;:align-items :center
                    :margin "4px -1px 4px 0"
                    :padding-left 3
                    :flex-grow 1
                    ":focus" {:background "var(--lumo-primary-color-50pct)"}
                    }
                   {:display :flex
                    ;:display (if (params :inline) :inline-flex :flex)
                    :flex-grow 1
                    :border-radius "var(--lumo-border-radius)"
                    ":focus" {:background "var(--lumo-primary-color-50pct)"}}
                   )
    :locals {:expr-zip '(or (ctx :params :expr-zip) (ctx :scope :expr-zip))}
    :events-handler {:keyup on-block-keypress
                     :input-changed input-changed}
    :dom-events {:keyup {:event :keyup
                         :event-subscription/fns [:not-in-input]
                         }}
    :dom {:tag :div
          :attrs {:tabindex 0}
          :children
          [{:if
            {:cond (w/gctx :params :label)
             :then {:set-styles {:font-size "var(--lumo-font-size-xxs)"
                                 :min-width 26
                                 :text-align :center
                                 :margin-left -3
                                 :padding "0 4px"
                                 :line-height "30px"
                                 ;:border-right "1px solid var(--lumo-primary-color-50pct)"
                                 :color "var(--lumo-primary-color)" ;"var(--lumo-primary-contrast-color)"
                                 :font-family "Fira Code"
                                 :font-weight :bold
                                 }
                    :dom {:tag :div :text (w/gctx :params :label)}}}}
           {:set-styles {:flex-grow 1}
            :dom {:tag :div :children (w/gctx :params :children)}}
           ]}}

   :block-choice
   {:events-handler {:select select-handler
                     :open (fn [e _]
                             [[:delay (if (:value e) 0 200)
                               [[:set-local :opened (:value e)]]]])}
    :locals {:expr-zip (w/gctx :params :expr-zip)}
    :local-state {:phrase ""
                  :opened false}
    :set-styles {:position :relative
                 :flex-grow 1
                 :margin "4px 0"
                 }
    :dom
    {:tag :div
     :children
     [{:dom
       {:tag :div
        :children
        [{:set-styles (merge
                        input-styles
                        {})

          :bind-input {:to :phrase}

          :dom-events {:focus {:event :open :value true}
                       :blur {:event :open :value false}}

          :dom {:tag :input
                :attrs {:placeholder '(params :placeholder)}}}]}}


      {:if {:cond (w/gctx :scope :opened)
            :then
            {:set-styles {:position :absolute
                          :top 26
                          :left 0
                          :width 300
                          :background-color "var(--lumo-base-color)"
                          :box-shadow "0 0 0 1px var(--lumo-shade-5pct), var(--lumo-box-shadow-m)"
                          :border-radius "var(--lumo-border-radius)"
                          :z-index 1
                          }
             :list-of {:items '((ctx :params :options) (ctx :scope :phrase))
                       :item-widget :block-choice-item}}}}
      ]}}

   :block-choice-item
   {:set-styles {:padding 5}
    :dom-events {:click {:event :select
                         :value (w/gctx :params :item :value)}}
    :dom {:tag :div
          :text (w/gctx :params :item :label)}}

   })