(ns core.engine
  (:require [core.incr :as incr]
            core.events
            core.styles))


;; ctx
;; :queue next
;; :stack already called

;; :before, :after, :error

(defprotocol IDisplayable
  (-display [this]))

(defn interceptors-execute-step [ctx]
  (let [stage (cond
                (::error ctx) :error
                (seq (::queue ctx)) :before
                :else :after)
        col (if (= stage :before) ::queue ::stack)]
    (if-let [next (peek (get ctx col))]
      (let [f (get next stage identity)
            ctx (if (= stage :before)
                  (-> ctx
                      (update ::queue pop)
                      (update ::stack conj next))
                  (-> ctx
                      (update ::stack pop)))]
        (try
          (f ctx)
          (catch js/Error e
            (-> ctx
                (assoc ::queue #queue [])
                (assoc ::error e)))))
      ctx)))

(defn execute-interceptors [ctx ics]
  (let [ctx (assoc ctx ::queue (into #queue [] ics)
                       ::stack [])]
    (loop [ctx ctx]
      (if (or (seq (::queue ctx))
              (seq (::stack ctx)))
        (recur (interceptors-execute-step ctx))
        ctx))))

#_(defn ti [x]
  {:before (fn [ctx]
             (update ctx :r conj [:before x]))
   :after (fn [ctx]
            (update ctx :r conj [:after x]))
   :error (fn [ctx]
            (-> ctx
                (update :r conj [:error x])
                ;(dissoc :error)
                )
            )})

#_(def te
  {:before (fn [ctx]
             (throw :err))})



(comment
  ; widget -> steps
  {:w-p {:text "asd"}
   :set-styles {:color "#111"}
   :add-events {"...." "...."}
   :order [:set-styles :add-events :w-p]

   :call {:widget :w1 :params {} :overwrite {}}
   })

(declare render-widget)

(defn deref-props [ctx m]
  ;; map-vals
  (into (with-meta {} (meta m))
        (map
          (fn [[k v]]
            (when-let [v (cond
                           (map? v) (deref-props ctx v)
                           (and (not (coll? v)) (not (keyword? v)) (ifn? v)) (v ctx) ; (deep-deref (v ..??
                           ;; binding *ctx* ?
                           v v)]
              [k v]))
          m)))

(defn set-params-of-interceptor [inter params]
  ;(println "set-params-of-interceptor" inter params)
  (->> inter
       (map (fn [[k v]]
              [k (fn set-params-of-interceptor [ctx] (v ctx (deref-props ctx params)))]))
       (into {})))

(declare deep-deref)

(defn call-interceptor [params]
  ;(println "call-interceptor" params)
  (let [prev-props (atom nil)]
    {:before (fn call-interceptor-before [ctx]
               (reset! prev-props (:props ctx))
               ;(println "call-interceptor" params)
               (if (:app/merge-props params)
                 (update ctx :props merge (deref-props ctx params))
                 (assoc ctx :props (deref-props ctx params))))
     :after (fn call-interceptor-after [ctx]
              ;(println "call-after" @prev-props)
              (-> ctx
                  ;; fixme: ctx getter passed to dom are derefed in wrong ctx
                  ;; but line below loops
                  ;(update :dom #(deep-deref ctx %))
                  ;; => not deref diff nodes (childeren), only getter/full-vals
                  (assoc :props @prev-props)))}))

(defn get-subparts [part parts-data]
  ;; fixme: order, dependency on ctx
  ;; probably remove :priority, depend on local :order
  (let [parts (get part :order)
        parts (or parts
                  (sort-by
                    #(- (or @(get-in parts-data [% :meta :priority]) 1))
                    (remove #{:meta} (keys part))))]
    (keep #(do [% (get part %)]) parts)))

(defn get-part-as-map [ctx part]
  (cond
    (keyword? part) @(get-in ctx [:parts part])
    (vector? part) @(get-in ctx (cons :parts part))
    :else part))

(defn get-interceptors [ctx part params]
  ;(println "get-interceptors" part params)
  (let [part (get-part-as-map ctx part)]
    (if-let [inter (:intercept part)]
      [(set-params-of-interceptor inter params)]
      (concat
        (when (not (empty? params)) [(call-interceptor params)])
        (mapcat
          (fn [[part pparams]] (get-interceptors ctx part pparams))
          (get-subparts part (:parts ctx)))))))

(defn widget-desc->dom-desc [ctx part params]
  (incr/thunk
    (fn wd->dd []
      ;(println "widget-desc->interceptors" part params)
      (let [interceptors (get-interceptors ctx part params)
            widget (execute-interceptors ctx interceptors)]
        ;(println "inter" interceptors)
        (:dom widget)))))

;(def widget-desc->dom-desc (first widget-desc->dom-desc*))
;(def widget-desc->dom-desc-memo (second widget-desc->dom-desc*))

(defn deref-one [ctx v]
  (cond
    (implements? incr/INode v) @v
    (implements? IDisplayable v) v
    (and (not (coll? v)) (not (keyword? v)) (ifn? v)) (v ctx)
    v v))

(defn deep-deref [ctx v]
  ;(println "deep-deref" v (meta v))
  (cond
    (map? v) (into (with-meta {} (meta v))
                   (keep
                     (fn [[k v]]
                       (when-let [v (deep-deref ctx v)] [k v]))
                     v))
    (implements? IDeref #_incr/INode v) (deep-deref ctx @v)
    (and (not (coll? v)) (not (keyword? v)) (ifn? v)) (deep-deref ctx (v ctx))
    :else v))

(defn render-widget [ctx w params]
  (incr/diff-val-thunk
    (fn render-widget' []
      ;(println "render-widget start" w)
      (let [res
            (deep-deref ctx @(incr/call-sub-thunk nil widget-desc->dom-desc ctx w params))]
        ;(println "render-widget" w res)
        res
        ))))

;(def render-widget (first render-widget*))
;(def render-widget-memo (second render-widget*))

(defn name-extend [name key]
  (if key
    (str name "/" key)
    name))

(def name->ctx (atom (sorted-map)))

(defn render-child [key ctx widget params widget-path]
  (println "render-child" key widget params (implements? IDisplayable widget) (type widget))
  (let [widget (if (implements? IDisplayable widget)
                 (-display widget)
                 widget)
        ctx (-> ctx
                (assoc :idx key)
                (update :widget/path (fn [p]
                                       (if (keyword? widget)
                                         [widget]
                                         (concat p widget-path))))
                (vary-meta update :incr/name name-extend key))]
    (swap! name->ctx assoc (-> ctx meta :incr/name) ctx)
    (incr/call-sub-thunk
      key
      render-widget
      ctx
      widget
      params)))

(defn dispose-node! [id]
  ;(incr/remove-memos-with-prefix render-widget-memo id)
  ;(incr/remove-memos-with-prefix widget-desc->dom-desc-memo id)
  (incr/remove-memos-with-prefix name->ctx id))

(defn node [parent prefix x]
  (let [node-changes (cond-> {}
                             (= x ::incr/deleted) ((fn [m]
                                                     (dispose-node! prefix)
                                                     (assoc m :remove-from parent)))
                             (:text x) (assoc :text (:text x))
                             (:events x) (assoc :events (:events x))
                             (:class x) (assoc :class (:class x))
                             (:styles x) (assoc :styles (:styles x))
                             (:value x) (assoc :value (:value x))
                             (:attrs x) (assoc :attrs (:attrs x))
                             (:tag x) (assoc :tag (:tag x)
                                             :parent parent))]
    (apply concat
           (when-not (empty? node-changes) [[prefix node-changes]])
           (when (coll? (:children x))
             (->> (:children x)
                  (sort-by first)
                  (keep (fn [[k v]]
                          (node prefix (name-extend prefix k) v)))
                  (not-empty))))))

(defn flat-dom [c]
  (incr/diff-thunk
    (fn []
      ;(println "NODE" @c)
      (node "root" "N" @c))))




;; unfold
;; map key -> thunk, to [k, v]