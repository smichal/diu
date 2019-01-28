(ns core.engine
  (:require [core.incr :as incr]))


(def w-p
  {:before (fn [ctx]
            (assoc ctx
              :dom {:tag :p
                    :text "asd" #_(get-in ctx [:params :text])})
            )})


(def w-styles
  {:after (fn [ctx]
            (assoc-in ctx [:dom :styles] (:w-styles ctx))
            )})

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

(defn ti [x]
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

(def te
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

#_(def container-children
  (incr/diff-val-memo
    (fn [children]
      (->> children
           (map (fn [[k v]]
                  [k (render-widget (:ctx ctx) v)]))
           (into {})))))

(def state-0
  {:transformers
   {:w-p {:after (fn [ctx params]
                   (assoc ctx
                     :dom {:tag :p
                           :text (:text params)}))}
    :set-styles {:after (fn [ctx params]
                          (assoc-in ctx [:dom :style] params))}
    :container {:after (fn [ctx params]
                         ;(println "CONT" (:children params))
                         #_(println "AAA" (map-indexed (fn [i v]
                                                         [i @(render-widget (:ctx ctx) v)])
                                                       (:children params)))
                         (assoc ctx
                           :dom {:tag :div
                                 :children
                                 (->> (:children params)
                                      (map (fn [[k v]]
                                             [k (render-widget ctx v)]))
                                      (into {}))
                                 #_(container-children (:children params))}))}

    ; list (coll, widget)

    :events-handler {:before (fn [ctx params]
                               (assoc ctx
                                 ::)
                               )}

    }

   :widgets
   {:w {:w-p {:text "abcd"}
        :set-styles {:color "#111"}
        :order [:set-styles :w-p]}
    :w2 {:w-p {:text "abcd"}
         :set-styles {:color "#111"}
         :order [:set-styles :w-p]}
    :w1 {:container {:children {0 :w
                                1 {:container {:children (->> (conj (vec (repeat 5 :w2)) :w)
                                                              (zipmap (range)))}
                                   :order [:container]}}}
         :order [:container]}
    }
   })


(def state-cell (incr/cell state-0))

(def ctx-0
  {:widgets (get state-cell :widgets)
   :transformers (get state-cell :transformers)
   })

(def widget-desc->dom-desc
  (incr/memo
    (fn [ctx w]
      ;(println "widget-desc->interceptors" ctx w)
      (let [widget-desc (if (keyword? w) @(get-in ctx [:widgets w])
                                         w)
            transformers (get widget-desc :order)
            interceptors (->> transformers
                              (mapv (fn [t]
                                      (let [trans @(get-in ctx [:transformers t])
                                            params (get widget-desc t)]
                                        (->> trans
                                             (map (fn [[k v]]
                                                    [k (fn [ctx] (v ctx params))]))
                                             (into {}))))))
            _ (println "CTX" ctx)
            widget (execute-interceptors ctx interceptors)]
        (:dom widget)))))


(declare deep-deref)

#_(def widget-desc->dom-desc'
  (incr/diff-val-memo
    (fn [ictx w]
      (deep-deref @(widget-desc->dom-desc ictx w)))))

(def render-widget
  (incr/diff-val-memo
    (fn [ctx w]
      ;(println "render-widget" w)
      (deep-deref @(widget-desc->dom-desc ctx w)))))



(defn deep-deref [m]
  ;(println "deep-deref" m (meta m))
  (if (map? m)
    (into (with-meta {} (meta m))
          (keep
            (fn [[k v]]
              (when-let [v (cond
                             (implements? incr/IThunk v) (deep-deref @v)
                             (map? v) (deep-deref v)
                             v v)]
                [k v]))
            m))
    m))

(defn node [parent prefix x]
  (let [node-changes (cond-> {}
                             (= x ::incr/deleted) (assoc :remove-from parent)
                             (:text x) (assoc :text (:text x))
                             (:tag x) (assoc :tag (:tag x)
                                             :parent parent))]
    (apply concat
           (when-not (empty? node-changes) [[prefix node-changes]])
           (->> (:children x)
                (sort-by first)
                (keep (fn [[k v]]
                        (node prefix (str prefix "-" k) v)))
                (not-empty)))))

(def flat-dom
  (incr/diff-memo
    (fn [c]
      ;(print "NODE" @c)
      (node "root" "N" @c))))

(defn flat-dom2 [node])


(defn tt []
  (let [rw (render-widget ctx-0 :w1)
        fd (flat-dom rw)
        obs (incr/diff-thunk (fn [] (print "delta " @fd)))
        ]
    (incr/add-parent! obs (incr/Observer.))
    (incr/stabilize!)
    (js/console.log rw)
    @rw))


;; unfold
;; map key -> thunk, to [k, v]