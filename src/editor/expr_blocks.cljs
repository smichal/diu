(ns editor.expr-blocks
  (:require [clojure.zip :as zip]
            [runtime.widgets :as w]
            [runtime.expr :as e]
            [incr.core :as incr]
            cljs.reader
            [parts.params :as params]
            ))

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

(declare input-styles)

(defn string-block [expr-zip ctx]
  (block :label "str"
         :expr-zip expr-zip
         :children
         [{:dom-events {:input {:event :input-changed}}
           :set-styles input-styles
           :dom {:tag :input
                 :attrs {:value (zip/node expr-zip)}
                 }}]
         ))

(declare fn-call getter-block)

(defn block-or-input [expr-zip spec ctx]
  (let [expr (zip/node expr-zip)]
    (cond
      (string? expr) (string-block expr-zip ctx)
      (and (list? expr) (= (first expr) 'ctx)) (getter-block expr-zip spec ctx)
      (list? expr) (fn-call expr-zip spec ctx)

      (nil? expr)
      (choice
        :expr-zip expr-zip
        :options
        (fn [phrase]
          (concat
            [{:text (str "\"" phrase "\"") :value phrase}
             (if-let [v (try (cljs.reader/read-string phrase)
                             (catch js/Error _ nil))]
               {:text (str "edn: " phrase) :value v})]
            (fns-list ctx)
            ))
        )

      :else {:dom {:tag :div :text (pr-str expr)}}
      )))

(defn children-zippers [zip]
  (take-while some? (iterate zip/right (zip/down zip))))

(defn append-child-zipper [zip]
  (-> zip
      (zip/append-child nil)
      (zip/down)
      (zip/rightmost)))

(defn getter-block [expr-zip spec ctx]
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
            (mapv #(block-or-input % spec ctx) arg-zips)
            [(block-or-input next-arg spec ctx)]
            ))))))

(defn string-input [expr-zip spec ctx]
  ;(js/console.log "string-input" expr-zip )
  (let [expr (zip/node expr-zip)]
   (cond
     (string? expr) (string-block expr-zip ctx)
     (number? expr) (string-block expr-zip ctx)
     (keyword? expr) (string-block expr-zip ctx)
     (and (list? expr) (= (first expr) 'ctx)) (getter-block expr-zip spec ctx)
     (list? expr) (fn-call expr-zip spec ctx)

     :else                                                  ; (nil? expr)
     (choice
       :expr-zip expr-zip
       :options
       (fn [phrase]
         (concat
           [{:text (str "\"" phrase "\"") :value phrase}
            (if-let [v (try (cljs.reader/read-string phrase)
                            (catch js/Error _ nil))]
              {:text (str "edn: " phrase) :value v})]
           (fns-list ctx)
           ))
       ))))

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
  (js/console.log "widget-call-block" expr-zip spec)
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
                                        ::w/instance-path (seq (conj (-> ctx :scope :ctx-of-widget-in-edit incr/value ::w/instance-path) (count (zip/lefts expr-zip))))}}}]}}
                 (params-block params-zip params-spec ctx :with-other-params true)
                 ]
      )))

(defn child-block [expr-zip spec ctx]
  ;(js/console.log "child-block" expr-zip)
  (let [val (zip/node expr-zip)]
    (if (or (< 1 (count val))
            (not= :widget (ffirst val)))
      (anonymous-widget expr-zip spec ctx)
      (widget-call-block (-> expr-zip (zip/down) (zip/down) (zip/right))
                         nil
                         ctx))))

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

(defn new-widgets-list [ctx]
  (concat
   (map
     (fn [[k v]]
       {:text (str "w: " k)
        :value {:widget {:widget k
                         :params (zipmap (w/infer-params-of-widget (get @(:runtime.widgets/widgets ctx) k))
                                         (repeat nil))}}})
     @(:runtime.widgets/widgets ctx))

   (keep
     (fn [[k v]]
       (when (:part/render v)
         {:text (str "p: " (or (:part/name v) k))
          :value {k (when (map? (:part/params v))
                      (->> (:part/params v)
                           (map (fn [[k v]]
                                  [k (:param/default v)]))
                           (into {})))}}))
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
                  [(choice
                     :expr-zip new-child-zip
                     :placeholder "new child..."
                     :options
                     (fn [phrase]
                       (new-widgets-list ctx)))]))))

(declare block-or-input2)

(defn map-entry-block [expr-zip spec ctx]
  ;(js/console.log "map-entry block" expr-zip spec)
  (let [[k v] (zip/node expr-zip)
        val-zip (last (children-zippers expr-zip))]
    (block
      :expr-zip expr-zip
      :children
      [{:set-styles {:margin 0}
        :editor.part-editor/field {:label (or (:param/name spec) (name k))
                                   :field (block-or-input2 val-zip (:param/type spec) ctx)}}
       ])))

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
        ]
    (block
      :label (when-not without-label "{}")
      :expr-zip expr-zip
      :children (concat
                  (map #(map-entry-block % nil ctx) zippers)
                  [(choice
                     :expr-zip new-child-zip
                     :placeholder "new map entry..."
                     :options keys-options)]))))

(defn params-block [expr-zip spec ctx & {:keys [with-other-params]}]
  (js/console.log "params-block" expr-zip spec)
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
                     (new-child-zip k)))]
    (block
      :expr-zip expr-zip
      :children (concat
                  (map (fn [[k spec]]
                         (map-entry-block (zipper k) spec ctx))
                       (merge
                         (when with-other-params
                           (zipmap (keys expr) (repeat nil)))
                         spec))
                  (when with-other-params
                    [(choice
                       :expr-zip new-child-zip
                       :placeholder "new param..."
                       :options keys-options)])))))


(def type->block (atom {}))

(defn register-block! [spec closed? f]
  (swap! type->block assoc spec [closed? f]))

(defn string-or-keyword? [x]
  (or (string? x) (keyword? x)))

(register-block! ::params/tag-name string-or-keyword? string-input)
(register-block! ::params/string string? string-input)
(register-block! ::params/children vector? children-block)
(register-block! ::params/child map? child-block)
(register-block! ::params/dom-attrs map? map-block)
(register-block! ::params/locals map? map-block)
(register-block! ::params/widget string-or-keyword? string-input)
(register-block! ::params/params map? map-block)
(register-block! ::params/widget-call map? widget-call-block)



(defn get-block-type [spec value]
  (let [[closed? block] (get @type->block spec)]
    (if (and closed? (closed? value))
      block
      (cond
        (map? spec) params-block
        :else (cond
                (map? value) map-block
                (string? value) string-input
                (number? value) string-input
                (keyword? value) string-input
                (boolean? value) boolean-input
                (vector? value) array-block
                (list? value) fn-call
                :else string-input   ;nil
                )
        ))))

(defn block-or-input2 [expr-zip spec ctx]
  (when-let [block (get-block-type spec (zip/node expr-zip))]
    (incr/incr block expr-zip spec ctx)))

(defn expr-input [expr spec ctx]
  ;(js/console.log "expr-input" expr spec)
  (let [expr-zip (expr-zipper expr)]
    (block-or-input2 expr-zip spec ctx)))


(extend-protocol IStack
  LazySeq
  (-peek [coll]
    ;(js/console.log "-peek" coll)
    (first coll))
  (-pop [coll] (rest coll)))

(defn select-handler [event ctx]
  (let [zipper (incr/value (get-in ctx [:scope :expr-zip]))]
    (js/console.log "select-handler" zipper (:value event) ctx)
    [[:emit-event {:event :expr-changed
                   :value (-> zipper
                              (zip/replace (:value event))
                              (zip/root))
                   :event/elem-call-id (:event/elem-call-id event) ;fixme
                   }]]))


(defn on-block-keypress [event ctx]
  (case (:event/key-pressed event)
    "Backspace" [[:emit-event {:event :expr-changed
                               :value (let [[_ path :as zipper] (incr/value (get-in ctx [:scope :expr-zip]))]
                                        (js/console.log "Backspace" zipper)

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
                              (zip/replace (:event/value event))
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
    :events-handler {:keydown on-block-keypress
                     :input-changed input-changed}
    :dom-events {:keydown {:event :keydown}}
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
          :text (w/gctx :params :item :text)}}

   })