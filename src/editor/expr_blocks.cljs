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

(defn choice [expr-zip options]
  {:block-choice {:expr-zip expr-zip
                  :options options}})

(defn fns-list [ctx]
  [{:text "fn: if"
    :value '(if)}

   {:text "params"
    :value '(ctx :params)}

   {:text "scope"
    :value '(ctx :scope)}

   ])

(defn string-block [expr-zip ctx]
  (block :label "\"...\""
         :expr-zip expr-zip
         :children
         [{:dom-events {:input {:event :input-changed}}
           :dom {:tag :input
                 :attrs {:value (zip/node expr-zip)}
                 }}]
         ))

(declare fn-call getter-block)

(defn block-or-input [expr-zip ctx]
  (let [expr (zip/node expr-zip)]
    (cond
      (string? expr) (string-block expr-zip ctx)
      (and (list? expr) (= (first expr) 'ctx)) (getter-block expr-zip ctx)
      (list? expr) (fn-call expr-zip ctx)

      (nil? expr)
      (choice
        expr-zip
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

(defn fn-call [expr-zip ctx]
  (let [[f & args] (zip/node expr-zip)
        arg-zips (drop 1 (children-zippers expr-zip))
        next-arg (append-child-zipper expr-zip)]

    (block
      :label "fn"
      :children
      (concat
       [{:dom {:tag :div :text (str f)}}]
       (mapv #(block-or-input % ctx) arg-zips)
       [(block-or-input next-arg ctx)]
       ))))


(defn getter-block [expr-zip ctx]
  (let [[_ k & path] (zip/node expr-zip)
        ;arg-zips (drop 1 (take-while some? (iterate zip/right (zip/down expr-zip))))
        next-arg (append-child-zipper expr-zip)

        dict (incr/incr get
                        (get-in ctx [:scope :ctx-of-widget-in-edit])
                        k)
        val @(incr/incr get-in dict path)
        next-keys (when (map? val) (keys val))
        ]

    (block
      :label "ctx"
      :children
      (concat
        [{:set-styles {:display :inline}
          :dom {:tag :div :text (str k " " (clojure.string/join " " (butlast path)))}}]

        (when path
          [(block
             :expr-zip (-> expr-zip (zip/down) (zip/rightmost))
             :label ""
             :children [{:dom {:tag :div :text (last path)}}]
             :inline true)])

        (when next-keys
         [(choice
            next-arg
            (fn [phrase]
              (map (fn [x]
                     {:text (str x) :value x})
                   next-keys)))])
        )))

  )

(defn string-input [expr-zip spec ctx]
  (js/console.log "string-input" expr-zip )
  (let [expr (zip/node expr-zip)]
   (cond
     (string? expr) (string-block expr-zip ctx)
     (and (list? expr) (= (first expr) 'ctx)) (getter-block expr-zip ctx)
     (list? expr) (fn-call expr-zip ctx)

     :else                                                  ; (nil? expr)
     (choice
       expr-zip
       (fn [phrase]
         (concat
           [{:text (str "\"" phrase "\"") :value phrase}
            (if-let [v (try (cljs.reader/read-string phrase)
                            (catch js/Error _ nil))]
              {:text (str "edn: " phrase) :value v})]
           (fns-list ctx)
           ))
       ))))

(declare map-block)

(defn anonymous-widget [expr-zip spec ctx]
  (let [parts (map key (zip/node expr-zip))]
    (block
      :label "w"
      :expr-zip expr-zip
      :children [
                 {:dom {:tag :div :text "Anonymous widget"}}
                 {:button {:text "edit"
                           :theme :tertiary
                           :onclick {:event :select-widget
                                     ::w/instance-path (conj (-> ctx :scope :ctx-of-widget-in-edit incr/value ::w/instance-path) (count (zip/lefts expr-zip)))} ;fixme?
                           }}
                 {:dom {:tag :div :text (pr-str parts)}}
                 ])))

(defn child-block [expr-zip spec ctx]
  (js/console.log "child-block" expr-zip)
  (let [[widget params] (first (zip/node expr-zip))
        widget-def (get @(::w/widgets ctx) widget)]
    (if-not widget-def
      (anonymous-widget expr-zip spec ctx)
      (let [params-zip (-> expr-zip (zip/down) (zip/down) (zip/right))
            params-zip (if (empty? params)
                         (let [widget-params (w/infer-params-of-widget widget-def)]
                           (-> params-zip (zip/replace (zipmap widget-params (repeat nil)))))
                         params-zip)]
        (block
          :label "w"
          :expr-zip expr-zip
          :children [
                     {:dom {:tag :div :text (pr-str widget)}}
                     {:button {:text "edit"
                               :theme :tertiary
                               :onclick {:event :select-widget
                                         ::w/instance-path (conj (-> ctx :scope :ctx-of-widget-in-edit incr/value ::w/instance-path) (count (zip/lefts expr-zip)))}}}
                     (map-block params-zip nil ctx)
                     ]
          )))))

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
                  (map #(child-block % ::params/child ctx) zippers)
                  [(choice
                     new-child-zip
                     (fn [phrase]
                       (map
                         (fn [[k v]]
                           {:text k :value {k {}}})
                         @(:runtime.widgets/widgets ctx))))]))))

(declare block-or-input2)

(defn map-entry-block [expr-zip spec ctx]
  (js/console.log "map-entry block" expr-zip)
  (let [[k v] (zip/node expr-zip)
        val-zip (last (children-zippers expr-zip))]
    (block
      :label ":"
      :expr-zip expr-zip
      :children
      [{:set-styles {:display :inline}
        :dom {:tag :div :text (name k)}}
       (block-or-input2 val-zip ::params/string ctx)
       ])))

(defn map-block [expr-zip spec ctx]
  (js/console.log "map block" expr-zip)
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
      :label "{}"
      :expr-zip expr-zip
      :children (concat
                  (map #(map-entry-block % nil ctx) zippers)
                  [(choice
                     new-child-zip
                     (fn [phrase]
                       [{:text (str ":" phrase) :value [(keyword phrase) nil]}]))]))))


(def type->block (atom {}))

(defn register-block! [spec f]
  (swap! type->block assoc spec f))

(register-block! ::params/tag-name string-input)
(register-block! ::params/string string-input)
(register-block! ::params/children children-block)
(register-block! ::params/child child-block)
(register-block! ::params/dom-attrs map-block)


(defn block-or-input2 [expr-zip spec ctx]
  (when-let [block (get @type->block spec)]
    (block expr-zip spec ctx)))

(defn expr-input [expr spec ctx]
  (js/console.log "expr-input" expr spec)
  (let [expr-zip (expr-zipper expr)]
    (block-or-input2 expr-zip spec ctx)))


(extend-protocol IStack
  LazySeq
  (-peek [coll]
    (js/console.log "-peek" coll)
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

(def widgets
  {
   :block
   {:set-styles {:background "var(--lumo-primary-color-10pct)"
                 :border "1px solid var(--lumo-primary-color)"
                 :border-radius "var(--lumo-border-radius)"
                 :padding "2px 0"
                 :display '(if (ctx :params :inline) :inline-flex :flex)
                 ;:align-items :center
                 ":focus" {:background "red"}
                 }
    :locals {:expr-zip '(or (ctx :params :expr-zip) (ctx :scope :expr-zip))}
    :events-handler {:keydown on-block-keypress
                     :input-changed input-changed}
    :dom-events {:keydown {:event :keydown}}
    :dom {:tag :div
          :attrs {:tabindex 0}
          :children
          [{:set-styles {:font-size "var(--lumo-font-size-xxs)"
                         :padding "2px 5px"
                         :border-right "1px solid var(--lumo-primary-color-50pct)"
                         :color "var(--lumo-primary-color)" ;"var(--lumo-primary-contrast-color)"
                         :font-family "Fira Code"
                         }
            :dom {:tag :div :text (w/gctx :params :label)}}
           {:dom {:tag :div :children (w/gctx :params :children)}}
           ]}}

   :block-choice
   {:events-handler {:select select-handler
                     ;:remove-zipper-node remove-handler
                     :open (fn [e _]
                             [[:delay 200
                               [[:set-local :opened (:value e)]]]])}
    :locals {:expr-zip (w/gctx :params :expr-zip)}
    :local-state {:phrase ""
                  :opened false}
    :set-styles {:position :relative}
    :dom
    {:tag :div
     :children
     [{:dom
       {:tag :div
        :children
        [{:set-styles {:border-radius "var(--lumo-border-radius)"
                       :background "var(--lumo-contrast-10pct)"
                       :padding "0 calc(0.375em + var(--lumo-border-radius) / 4 - 1px)"
                       :border 0
                       :font-size "var(--lumo-font-size-s)"
                       :height 26
                       }

          :bind-input {:to :phrase}

          :dom-events {:focus {:event :open :value true}
                       :blur {:event :open :value false}}

          :dom {:tag :input
                :attrs {:placeholder "Choose"}}}]}}


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