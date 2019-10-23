(ns runtime.expr
  (:require [runtime.widgets :as w]
            [incr.core :as incr]))


(deftype CtxGetter [path]
  w/IWithCtx
  (-with-ctx [this ctx]
    (get-in ctx path)))

(defn ctx-get [& path]
  (CtxGetter. path))


(defn parts-for-search [parts]
  (->> parts
       (map (fn [[k v]]
              {:name (or (:part/name v) (str k))
               :desc (or (:part/desc v) "")}
              ))))

(def env
  {'= =
   'str str
   'pr-str pr-str
   'get-in get-in
   'map map
   'mapcat mapcat

   'parts-for-search parts-for-search
   })

(defn eval-expr [ctx expr]
  (let [res
        (cond

          (env expr) (env expr)

          (and (list? expr)
               (= 'if (first expr)))
          (let [[_ c t e] expr]
            (if (eval-expr ctx c)
              (eval-expr ctx t)
              (eval-expr ctx e)))

          (and (list? expr)
               (= 'or (first expr)))
          (let [[_ & args] expr]
            (first (keep #(eval-expr ctx %) args)))

          (and (list? expr)
               (= 'and (first expr)))
          (let [[_ & args] expr]
            (every? #(eval-expr ctx %) args))


          (and (list? expr) (= 'ctx (first expr)))
          (let [[_ & path] expr]
            (when (or (< 1 (count path))
                      (not (#{:params :scope} (first path)))) ;; todo remove
             (reduce
               (fn [o k] (incr/value (get o k)))
               ctx
               (map (partial eval-expr ctx) path))))

          (list? expr)
          (let [[f & args] expr]
            (apply (eval-expr ctx f)
                   (->> args
                        (map (partial eval-expr ctx)))))

          (vector? expr)
          (mapv (partial eval-expr ctx) expr)

          (map? expr)
          (->> expr
               (map (fn [[k v]]
                      [(eval-expr ctx k) (eval-expr ctx v)]))
               (into {}))

          ;(implements? w/IWithCtx expr)
          ;(w/-with-ctx expr ctx)

          :else
          expr)
        res (incr/value res)
        res (if (implements? IDeref res) nil res)
        ]
    ;(js/console.log "=>" expr " => " res)
    res
    ))

(deftype Expr [expr]
  w/IWithCtx
  (-with-ctx [this ctx]
    ;(js/console.log "EVAL" expr ctx)
    (incr/incr eval-expr ctx expr))
  IEquiv
  (-equiv [o other]
    (and (= (type o) (type other))
         (= expr (.-expr other))))
  IHash
  (-hash [this]
    (-hash expr)))


(defn expr [e]
  (Expr. e))



(defn block-args-for-fn [f expr])

{:label "fn"
 :type :call
 :args (fn [ctx expr phrase]
         (let [[f & args] expr]
           (if-not f
             [{:options [{:label "if"
                          :value 'if}]}
              ]
             (concat
               [{:value f}]
               (block-args-for-fn f expr)
               )
             )))
 :emit-expr (fn [args] ; expr -> expr
              (apply list args))
 ;:parse-expr (fn []) ; expr -> choices
 }



{:label "widget"
 :type :widget-call
 :args {:widget {:widget (fn [])
                 :params (fn [])}}
 }

{:label "{}"
 :type :styles
 :args {:margin nil
        ::add (fn [])}
 }

{:label "{}"
 :type :event
 :args {:event (fn [])}}

{:label "[]"
 :type :children
 :args []
 }