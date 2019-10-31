(ns runtime.widgets
  (:require [clojure.spec.alpha :as s]
            clojure.test.check.generators
            clojure.test.check.properties
            clojure.test.check
            [incr.core :as incr]
            [com.rpl.specter :as specter]
            [runtime.expr :as expr]))

(s/def ::part
  (s/keys
    :req [(or :part/render :part/augment-ctx :part/augment-result)]

    ))

(s/def ::result (s/keys))
(s/def ::ctx (s/keys))

(s/def :part/name string?)
(s/def :part/desc string?)
#_(s/def :part/render (s/fspec :args (s/cat :ctx ::ctx
                                          :params (s/map-of keyword? any?))
                             :ret ::result))
#_(s/def :part/augment-ctx (s/fspec :args (s/cat :ctx ::ctx
                                               :params (s/map-of keyword? any?))
                                  :ret ::ctx))
#_(s/def :part/augment-result (s/fspec :args (s/cat :ctx ::ctx
                                                  :params (s/map-of keyword? any?)
                                                  :result ::result)
                                     :ret ::result))

;(s/def :part/params (s/map-of keyword? :part/param))
;(s/def :part/param (s/keys))

(defprotocol IWithCtx
  (-with-ctx [this ctx]))

(deftype WithCtx [f]
  IWithCtx
  (-with-ctx [this ctx]
    (f ctx)))

(defn with-ctx [f] (WithCtx. f))

(defn deep-eval-params [ctx x]
  (specter/transform
    (specter/codewalker #(or (list? %)
                             (implements? IWithCtx %)
                             (:zip/branch? (meta %))        ;fixme
                             ))
    #(cond
       (list? %) (incr/incr expr/eval-expr ctx %)
       (implements? IWithCtx %) (-with-ctx % ctx)
       :else %)
    x))

(def parts (atom {}))

(defn defpart [id & {:as m}]
  (when-not (s/valid? ::part m)
    (js/console.warn m (s/explain-str ::part m))
    (throw (js/Error. "Part invalid")))
  (swap! parts assoc id m))



#_(s/def ::widget
  (s/keys
    :opt [:widget/meta
          :order]
    ))

(defn map-in-order [m]
  (let [order (:order m)
        not-ordered (clojure.set/difference (set (keys m)) (set order) #{:order :meta})]
    (concat
      (map (fn [k] [k (m k)]) order)
      (map (fn [k] [k (m k)]) not-ordered))))

(def call-id->ctx (atom {}))
(def last-call-id (atom 0))
(defn next-call-id [] (swap! last-call-id inc))

(declare call resolve-widget)

;; parts from ctx?
(defn compose-parts [ctx w]
  ;(js/console.log "compose-parts" (::param-path ctx)  w ctx)
  (let [parts-defs (::parts ctx)
        part-calls (map-in-order w)
        parts-num (count part-calls)
        part-calls (map-indexed (fn [idx [id params]]
                          (let [part (get parts-defs id)]
                            (if-not part
                              (if (not= idx (dec parts-num))
                                (throw (js/Error. (str "Part not found: " id)))
                                ;; widget can be called instead of part for last part
                                [(get parts-defs :widget)
                                 {:widget id :params params}])
                              [part params])))
                        part-calls)
        [last-part last-part-params] (last part-calls)
        call-id (hash (::instance-path ctx))
        ;_ (js/console.log "hash" (::instance-path ctx) (hash (::instance-path ctx)))
        ctx (assoc ctx
              ;::parent-call-id (::call-id ctx) ;; passing call-id from parent causes additional calls to subcomponents
              ::call-id call-id)
        ]

    (let [ctx (reduce
                (fn [ctx [part params]]
                  (if-let [f (:part/augment-ctx part)]
                    (f ctx
                       (deep-eval-params ctx params))
                    ctx))
                ctx
                part-calls)

          _ @(incr/incr (incr/on-destroy (fn []
                                           ;(js/console.log "DESTROY" call-id)
                                           (swap! call-id->ctx (fn [m]
                                                                 (if (= (m call-id) ctx)
                                                                   (dissoc m call)
                                                                   m))))))
          _ (swap! call-id->ctx assoc call-id ctx)
          ;_ (js/console.log call-id "=>" ctx)

          _ (when-not (:part/render last-part)
              (throw (js/Error. (str "Widget without :part/render fn: " w))))

          result ((:part/render last-part)
                  ctx
                  (deep-eval-params ctx last-part-params)) ;; auto resolve widget?
          result (reduce
                   (fn [result [part params]]
                     (if-let [f (:part/augment-result part)]
                       (f ctx
                          (deep-eval-params ctx params)
                          result)
                       result))
                   result
                   (reverse part-calls))]
      result)))

(defn get-or-throw [m k msg]
  (if-let [r (get m k)]
    r
    (throw (js/Error. msg))))

(defn resolve-widget [ctx widget-or-id]
  ;(println "resolve-widget" widget-or-id)
  (if (keyword? widget-or-id)
    (incr/incr get-or-throw (::widgets ctx) widget-or-id (str "Widget not found " widget-or-id))
    widget-or-id))

(defn spy [msg x]
  (js/console.log msg x)
  x)

(defn call [ctx w key]
  ;(js/console.log "CALL" w (:path (meta w)))
  (when w
   (try
     (with-meta
       (incr/deep-deref
         @(incr/incr compose-parts
                     (-> ctx
                         (assoc ::param-path (:path (meta w)))
                         (update ::instance-path conj key)
                         (dissoc ::call-id))
                     (incr/value w)))
       {:derefed true}
       )
     (catch js/Error e
       (js/console.error e)
       {:dom/tag :div
        :dom/text (pr-str e)}
       )
     )))

;; :part/render (ctx: const), (params: (possible?) thunk)
;; children (id -> Thunk(x)) ?


(deftype CtxGetter [path]
 IWithCtx
  (-with-ctx [this ctx]
    (get-in ctx (deep-eval-params ctx path)))
  ;Object
  ;(toString [this] (pr-str path))
  IEquiv
  (-equiv [this other]
    (= path (.-path other)))
  ;IPrintWithWriter
  ;(-pr-writer [_ writer _]
  ;  (write-all writer "#ctx" (pr-str (vec path))))
  )

(defn gctx [& args]
  (CtxGetter. args))


(defn with-path-annotation [prefix m]
  (if (implements? IMeta m)
    (vary-meta
      (cond
        (map? m)
        (->> m
             (map (fn [[k v]]
                    [k (with-path-annotation (conj prefix k) v)]))
             (into {}))
        (vector? m)
        (vec (map-indexed (fn [i x] (with-path-annotation (conj prefix i) x)) m))
        :else
        m)
      assoc
      :path prefix)
    m))

(defn infer-params-of-widget [w]
  (map
    second
    (specter/select
      (specter/walker #(and (list? %) (= 'params (first %))))
      w)))