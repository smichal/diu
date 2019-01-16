(ns core.engine
  (:require [core.incr :as incr]
            core.events
            core.styles))


;; ctx
;; :queue next
;; :stack already called

;; :before, :after, :error

(defn interceptors-execute-step [ctx]
  (let [stage (cond
                (:error ctx) :error
                (seq (:queue ctx)) :before
                :else :after)
        col (if (= stage :before) :queue :stack)]
    (if-let [next (peek (get ctx col))]
      (let [f (get next stage identity)
            ctx (if (= stage :before)
                  (-> ctx
                      (update :queue pop)
                      (update :stack conj next))
                  (-> ctx
                      (update :stack pop)))]
        (try
          (f ctx)
          (catch js/Error e
            (-> ctx
                (assoc :queue #queue [])
                (assoc :error e)))))
      ctx)))

(defn execute-interceptors [ctx ics]
  (let [ctx (assoc ctx :queue (into #queue [] ics)
                       :stack [])]
    (loop [ctx ctx]
      (if (or (seq (:queue ctx))
              (seq (:stack ctx)))
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
              [k (fn [ctx] (v ctx (deref-props ctx params)))]))
       (into {})))

(defn call-interceptor [params]
  ;(println "call-interceptor" params)
  (let [prev-props (atom nil)]
    {:before (fn [ctx]
               (reset! prev-props (:props ctx))
               ;(println "call-interceptor" (deref-props ctx params))
               (assoc ctx :props (deref-props ctx params)))
     :after (fn [ctx]
              (assoc ctx :props @prev-props))}))

(defn get-subparts [part]
  (let [parts (get part :order (keys part))]
    (keep #(do [% (get part %)]) parts)))

(defn get-interceptors [ctx part params]
  ;(println "get-interceptors" part params)
  (let [part (cond
               (keyword? part) @(get-in ctx [:parts part])
               (vector? part) @(get-in ctx (cons :parts part))
               :else part)]
    (if-let [inter (:intercept part)]
      [(set-params-of-interceptor inter params)]
      (concat
        (when (not (empty? params)) [(call-interceptor params)])
        (mapcat
          (fn [[part pparams]] (get-interceptors ctx part pparams))
          (get-subparts part))))))

(def widget-desc->dom-desc*
  (incr/memo*
    (fn wd->dd [ctx part params]
      ;(println "widget-desc->interceptors" part)
      (let [
            ;widget-desc (if (keyword? w) @(get-in ctx [:parts w]) w)
            ;parts (get widget-desc :order (keys widget-desc))
            interceptors (get-interceptors ctx part params)
            #_(->> transformers
                              (mapv (fn [t]
                                      (let [trans @(get-in ctx [:transformers t])
                                            params (deref-props ctx (get widget-desc t))]
                                        (->> trans
                                             (map (fn [[k v]]
                                                    [k (fn [ctx] (v ctx params))]))
                                             (into {}))))))
            ;_ (println "CTX" ctx)
            widget (execute-interceptors ctx interceptors)]
        (:dom widget)))))

(def widget-desc->dom-desc (first widget-desc->dom-desc*))
(def widget-desc->dom-desc-memo (second widget-desc->dom-desc*))

(defn deref-one [ctx v]
  (cond
    (implements? incr/INode v) @v
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
    :else v)

  #_(if (map? m)
    (into (with-meta {} (meta m))
          (keep
            (fn [[k v]]
              (when-let [v (cond
                             (implements? IDeref #_incr/INode v) (deep-deref ctx @v)
                             (map? v) (deep-deref ctx v)
                             (and (not (coll? v)) (not (keyword? v)) (ifn? v)) (deep-deref ctx (v ctx)) ; (deep-deref (v ..??
                             ;; binding *ctx* ?
                             v v)]
                [k v]))
            m))
    m))

(def render-widget*
  (incr/diff-val-memo*
    (fn render-widget' [ctx w params]
      ;(println "render-widget start" w)
      (let [res
            (deep-deref ctx @(widget-desc->dom-desc ctx w params))]
        (println "render-widget" w res)
        res
        ))))

(def render-widget (first render-widget*))
(def render-widget-memo (second render-widget*))

(defn name-extend [name key]
  (str name "/" key))

(def name->ctx (atom (sorted-map)))

(defn render-child [key ctx widget params widget-path]
  ;(println "render-child" key ctx widget)
  (let [ctx (-> ctx
                (assoc :idx key)
                (update :widget/path (fn [p]
                                       (if (keyword? widget)
                                         [widget]
                                         (concat p widget-path))))
                (vary-meta update :incr/name name-extend key))]
    (swap! name->ctx assoc (-> ctx meta :incr/name) ctx)
    (render-widget
      ctx
      widget
      params)))

(defn dispose-node! [id]
  (incr/remove-memos-with-prefix render-widget-memo id)
  (incr/remove-memos-with-prefix widget-desc->dom-desc-memo id)
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
                             (:tag x) (assoc :tag (:tag x)
                                             :parent parent))]
    (apply concat
           (when-not (empty? node-changes) [[prefix node-changes]])
           (->> (:children x)
                (sort-by first)
                (keep (fn [[k v]]
                        (node prefix (name-extend prefix k) v)))
                (not-empty)))))

(def flat-dom
  (incr/diff-memo
    (fn [c]
      ;(println "NODE" @c)
      (node "root" "N" @c))))




;; unfold
;; map key -> thunk, to [k, v]