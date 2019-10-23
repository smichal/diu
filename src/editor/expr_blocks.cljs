(ns editor.expr-blocks
  (:require [clojure.zip :as zip]
            [runtime.widgets :as w]
            [runtime.expr :as e]
            [incr.core :as incr]
            cljs.reader
            ))

(defn expr-zipper [expr]
  (zip/zipper
    (fn [x]
      (or (list? x) (vector? x) (map? x)))
    seq
    (fn [node children]
      (cond
        (vector? node) (with-meta (vec children) (meta node))
        (map? node) (with-meta (into {} children) (meta node))
        (or (list? node) (seq? node)) (with-meta (apply list children) (meta node))
        ))
    expr))


(defn block [& {:keys [label children]}]
  {:block
   {:label label
    :children children}})

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
         :children
         [{:dom-events {:input {:event :expr-changed}}
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

(defn fn-call [expr-zip ctx]
  (let [[f & args] (zip/node expr-zip)
        arg-zips (drop 1 (take-while some? (iterate zip/right (zip/down expr-zip))))
        next-arg (-> expr-zip
                     (zip/append-child nil)
                     (zip/down)
                     (zip/rightmost))]

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
        next-arg (-> expr-zip
                     (zip/append-child nil)
                     (zip/down)
                     (zip/rightmost))

        dict (incr/incr get
                        (get-in ctx [:scope :ctx-of-widget-in-edit])
                        k)
        val @(incr/incr get-in dict path)
        next-keys (when (map? val) (keys val))
        ]

    (js/console.log "GETTER" (get-in ctx [:scope :ctx-of-widget-in-edit]))
    (js/console.log "GETTER" val next-keys)

    (block
      :label "ctx"
      :children
      (concat
        [{:dom {:tag :div :text (str k)}}
         {:dom {:tag :div :text (pr-str path)}}]
        (when next-keys
         [(choice
            next-arg
            (fn [phrase]
              (map (fn [x]
                     {:text (str x) :value x})
                   next-keys)))])
        )))

  )

(defn string-input [expr ctx]
  (let [expr (if (instance? e/Expr expr)
               (.-expr expr)
               expr)
        expr-zip (expr-zipper expr)]
    (cond
      (string? expr) (string-block expr-zip ctx)
      (and (list? expr) (= (first expr) 'ctx)) (getter-block expr-zip ctx)
      (list? expr) (fn-call expr-zip ctx)

      :else                                                 ; (nil? expr)
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

(extend-protocol IStack
  LazySeq
  (-peek [coll]
    (js/console.log "Peed" coll)
    (first coll))
  (-pop [coll] (rest coll)))

(defn select-handler [event ctx]
  (let [zipper (incr/value (get-in ctx [:scope :expr-zip]))]
    [[:emit-event {:event :expr-changed
                   :value (-> zipper
                              (zip/replace (:value event))
                              (zip/root)
                              e/expr)
                   :event/elem-call-id (:event/elem-call-id event) ;fixme
                   }]]))

; todo
#_(defn remove-handler [event ctx]
  (let [zipper (incr/value (get-in ctx [:scope :expr-zip]))]
    [[:emit-event {:event :expr-changed
                   :value (-> zipper
                              (zip/replace (:value event))
                              (zip/root)
                              e/expr)
                   :event/elem-call-id (:event/elem-call-id event) ;fixme
                   }]])
  )

(def widgets
  {
   :block
   {:set-styles {:background "var(--lumo-primary-color-10pct)"
                 :border "1px solid var(--lumo-primary-color)"
                 :border-radius "var(--lumo-border-radius)"
                 :padding "2px 0"
                 :display :flex
                 ;:align-items :center
                 }
    :dom {:tag :div
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
             :list-of {:items (e/expr '((ctx :params :options) (ctx :scope :phrase)))
                       :item-widget :block-choice-item}}}}
      ]}}

   :block-choice-item
   {:set-styles {:padding 5}
    :dom-events {:click {:event :select
                         :value (w/gctx :params :item :value)}}
    :dom {:tag :div
          :text (w/gctx :params :item :text)}}

   })