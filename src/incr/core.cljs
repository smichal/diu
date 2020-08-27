(ns incr.core
  (:require [com.rpl.specter :as specter])
  )

(def ^:dynamic *height-limit* 100000)
(def ^:dynamic *referred-subscriptions* nil)
(def ^:dynamic *parent-node* nil)
(def dirty-nodes #js [])
(def current-time (atom 1))

(defn value [x]
  (if (implements? IDeref x)
    @x
    x))

; [sth args] -> node
; node = {:value, :state, :ref-count. :height}
(def nodes (atom {}))

; recalculate-result - value, state, propagate (-to) (bool/set)

(defprotocol IIncr
  (calculate [this args state])
  (destroy [this]))

; thunk - deref
(defn- new-node []
  {:value nil
   :state nil
   :height nil
   :dependencies #{}
   :subscriptions #{}
   })

(defn get-node [n]
  (or (get @nodes n)
      (new-node)))

(defn remove-dep [[x args :as node] sub]
  (swap! nodes update-in [node :subscriptions] disj sub)
  (when (empty? (get-in @nodes [node :subscriptions]))
    (doseq [x (get-in @nodes [node :dependencies])]
      (remove-dep x node))
    (destroy x)
    (swap! nodes dissoc node)))

(defn add-dep [node sub]
  (swap! nodes update-in [node :subscriptions] conj sub))

(defn to-dirty-nodes! [ns]
  (doseq [node ns]
    (let [h (get-in @nodes [node :height] 0)]
      (if-let [s (aget dirty-nodes h)]
        (aset dirty-nodes h (conj s node))
        (aset dirty-nodes h #{node})))))


(defn recalculate-node [[x args :as n]]
  ;(js/console.log "recalculate-node" n)
  (binding [*parent-node* n
            *referred-subscriptions* (volatile! #{})]
    (let [node (get-node n)
          [val state propagate?] (calculate x args (:state node))

          old-deps (:dependencies node)
          new-deps @*referred-subscriptions*
          added (clojure.set/difference new-deps old-deps)
          removed (clojure.set/difference old-deps new-deps)

          node (assoc node
                 :value val
                 :state state
                 :dependencies new-deps
                 :height (inc (apply max -1 (map #(get-in @nodes [% :height]) new-deps)))
                 :recompute-time @current-time
                 )]

      (doseq [x removed] (remove-dep x n))
      (doseq [x added] (add-dep x n))

      (swap! nodes assoc n node)
      ;(js/console.log "ADD" n node)

      (if (coll? propagate?)
        (to-dirty-nodes! propagate?)
        (when propagate?
          (to-dirty-nodes! (:subscriptions node)))))))

(declare stabilize-to-height!)

(defn get-node-value [n]
  (let [node (get-node n)
        parent-height (or (get (get-node *parent-node*) :height) *height-limit*)]
    (when *referred-subscriptions*
      (vswap! *referred-subscriptions* conj n))

    (if (and (:height node)
             (or (< (:height node) parent-height)
                 (= (:recompute-time node) @current-time)))
      (:value node)
      (do
        (recalculate-node n)
        (:value (get-node n)))
      #_(do
        (loop [height (:height (get-node n))]
          (swap! nodes assoc-in [*parent-node* :height] (inc height))
          (stabilize-to-height'! (:height node))
          (let [new-height (:height (get-node n))]
            (when (< height new-node)
              (recur new-height))))
        (:value (get-node n))))))

(deftype IncrValue [x args]
  IDeref
  (-deref [this]
    (let [v (get-node-value [x args])]
     (if (instance? js/Error v)
       (throw v)
       v)))
  IEquiv
  (-equiv [o other]
    (and (= (type o) (type other))
         (= x (.-x other))
         (= args (.-args other))))
  IHash
  (-hash [this]
    (hash [x args])))

#_(deftype IncrMap [x args]
  IDeref
  (-deref [this]
    (get-node-value [x args]))
  cljs.core/ILookup
  (-lookup [this k not-found] (or (-get this k) not-found))
  (-lookup [this k] (-get this k))
  )

(defprotocol IncrValueConstructor
  (-create [x args]))

(extend-protocol IncrValueConstructor
  default
  (-create [x args] (IncrValue. x args))
  ;PersistentHashMap
  ;(-create [x args] (IncrMap. x args))
  )

(defn incr [x & args]
  (-create x args)
  #_(let [t (IncrValue. x args)]
    ;(to-dirty-nodes! [[x args]])
    t))

(defn stabilize-to-height! [h]
  (loop [i 0]
    ;(println "stabilize " i (aget dirty-nodes i))
    (while (not (empty? (aget dirty-nodes i)))
      (let [t (first (aget dirty-nodes i))]
        (aset dirty-nodes i (disj (aget dirty-nodes i) t))
        (recalculate-node t)))
    (when (and (< i h)
               (< i (.-length dirty-nodes)))
      (recur (inc i)))))

(defn stabilize! []
  (swap! current-time inc)
  (stabilize-to-height! *height-limit*)
  )

(def scheduled? (atom false))
(defn schedule-stabilization! []
  (when-not @scheduled?
    (reset! scheduled? true)
    (js/setTimeout (fn []
                     (reset! scheduled? false)
                     ;(js/console.profile "stabilize!")
                     (stabilize!)
                     ;(js/console.profileEnd "stabilize!")
                     )
                   1)))

(deftype IncrCell [^:mutable v]
  IDeref
  (-deref [this]
    (get-node-value [this]))
  IReset
  (-reset! [this new-val]
    (set! v new-val)
    (to-dirty-nodes! [[this]]))
  ISwap
  (-swap! [this f] (-reset! this (f v)))
  (-swap! [this f a] (-reset! this (f v a)))
  (-swap! [this f a b] (-reset! this (f v a b)))
  (-swap! [this f a b xs] (-reset! this (f v a b xs)))      ;; apply??
  IIncr
  (calculate [this args state]
    [v v (not= state v)])
  (destroy [this] (set! v nil)))

(defn cell [v]
  (let [x (IncrCell. v)]
    (to-dirty-nodes! [[x]])
    x))

(extend-protocol IIncr
  js/Function
  (calculate [f args state]
    (let [v (try
              (apply f (map value args))
              (catch js/Error e
                e))]  ;; map value?
      [v v (not= state v)]))
  (destroy [this]))

(defn try-deref [v]
  (try
    (deref v)
    (catch js/Error e
      e)))

(defn deep-deref [x]
  (specter/transform
    (specter/walker #(do
                       #_(when (:derefed (meta %))
                         (js/console.log "DEREFED"))
                       (or
                         (:derefed (meta %))
                         (implements? IDeref %))))
    ;deref
    value
    x))

(def aaa #js{:prototype map})

(extend-protocol IIncr
  aaa
  (calculate [map' [f coll] {:keys [input output]}]
    (let [prev (or input [])
          coll (value coll)
          res (map
                (fn [a b old]
                  (if (= a b)
                    old
                    (f a)))
                coll
                (concat prev (repeat nil))
                (concat output (repeat nil)))]
      [res
       {:input coll :output res}
       (not= res output)]))
  (destroy [this])
  )

(defn on-destroy [f]
  (reify
    IIncr
    (calculate [this args state] [nil nil false])
    (destroy [this] (f))))



#_(extend-protocol PersistentHashMap
  js/Function
  (calculate [m _ {:keys [val ]}]
    (if (nil? val)
      (let [subnodes (into {} (traverse-incr-map [] def))
            val (reduce (fn [m [node path]]
                          (assoc-in m path (exception-as-value #(do @node))))
                        def
                        subnodes)]
        [val {:val val :subnodes subnodes} true])
      (doseq [s (:dependencies (get-node [m nil]))]
        ;(js/console.log "REC" s (.-change-time s) @current-time)
        (when (= @current-time (.-change-time s))
          (let [node (.-node s)]
            (set! val (assoc-in val (subnodes node) (exception-as-value #(do @node))))))))

    (let [v (apply f (map value args))]
      [v v (not= state v)]))
  (destroy [this]))
