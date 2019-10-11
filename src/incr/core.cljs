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
       v))))

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
                     (stabilize!))
                   1)))

(deftype IncrCell [^:mutable v]
  IDeref
  (-deref [this]
    (get-node-value [this]))
  IReset
  (-reset! [this new-val]
    (set! v new-val)
    (to-dirty-nodes! [[this]]))
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
    (specter/walker #(implements? IDeref %))
    deref
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



#_(def aaa
  (reify IIncr
    (calculate [f args state]
      (println "QQQQ" )
      )
    (destroy [this])
    ))

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

(comment



  ;; bookkeeping przeliczania
  ;; observable value - gettery dla map, itd
  ;; tworzenie wartosci, fn/getter/cell/hash-map

  (comment

    (incr/cell v)
    (incr/thunk
      (+ @v 1))
    (incr/hash-map
      {:a v
       :b ^:const {:c 2}})
    (get (incr/cell {:a 1}) :a)
    )

  (def ^:dynamic *height-limit* 100000)
  (def ^:dynamic *referred-subscriptions* nil)
  (def ^:dynamic *parent-node* nil)

  (def dirty-nodes #js [])
  (def current-time (atom 1))

  (deftype OutOfHeight [h])
  (deftype RestartStabilization [])

  (defn to-dirty-nodes! [nodes]
    (doseq [node nodes]
      (let [h (.-height (.-node-meta node))]
        (println "to-dirty" h node)
        (if-let [s (aget dirty-nodes h)]
          (aset dirty-nodes h (conj s node))
          (aset dirty-nodes h #{node})))))

  (defprotocol INode
    (-recompute [this])
    (-get [this filter]))

  (defprotocol ISubVal
    (-subval-get [this node filter]))

  (deftype Subscription
    [node
     filter
     observer
     ^:mutable change-time]
    IEquiv
    (-equiv [o other]
      (and (= node (.-node other))
           (= filter (.-filter other))
           (= observer (.-observer other))))
    IHash
    (-hash [o] (-hash [node filter observer])))

  (deftype NodeMeta
    [^:mutable subscriptions                                  ;; filter -> node
     ^:mutable dependencies                                   ;; filter, node
     ^:mutable height
     ^:mutable force-active
     ])

  (defprotocol ICell)

  (declare node-return-effects!)
  (declare cell-set!)

  (deftype Cell
    [^:mutable val
     ^:mutable prev-val
     node-meta]
    ICell
    INode
    (-recompute [this]
      ;; return changed?
      true
      )
    (-get [this filter]
      (-subval-get val this filter))
    IDeref
    (-deref [this]
      (-get this nil))
    cljs.core/ILookup
    (-lookup [this k not-found] (or (-get this k) not-found))
    (-lookup [this k] (-get this k))
    ISwap
    (-swap! [this f] (cell-set! this (f val)))
    (-swap! [this f a] (cell-set! this (f val a)))
    (-swap! [this f a b] (cell-set! this (f val a b)))
    (-swap! [this f a b xs] (cell-set! this (f val a b xs)))  ;; & apply ?
    IReset
    (-reset! [this v]
      (cell-set! this v)))

  (defn empty-node-meta [height]
    (NodeMeta. #{} #{} height false))

  (defn cell [v]
    (Cell. v nil (empty-node-meta 0)))

  (defn changed? [old new]
    (not= old new))

  (defn exception-as-value [f]
    (try
      (f)
      (catch js/Error e
        e)))


  ;; when not deps after (f) its should be marked as const
  (deftype Thunk
    [f
     ^:mutable val
     ^:mutable prev-val
     node-meta
     id]
    INode
    (-recompute [this]
      (let [new-val (exception-as-value f)
            changed (changed? val new-val)]
        ;(set! prev-val val)
        (set! val new-val)
        changed))
    (-get [this filter]
      (let [v (-subval-get val this filter)]
        (if (instance? js/Error v)
          (throw v)
          v)))
    IDeref
    (-deref [this]
      (-get this nil))
    cljs.core/ILookup
    (-lookup [this k not-found] (or (-get this k) not-found))
    (-lookup [this k] (-get this k))
    )

  (def id-counter (atom 0))

  (defn thunk-f [f]
    (Thunk. f nil nil (empty-node-meta 1)
            (str "#" (swap! id-counter inc))))


  (defn traverse-incr-map [path m]
    (mapcat
      (fn [[k v]]
        (cond
          (map? v) (traverse-incr-map (conj path k) v)
          (implements? INode v) [[v (conj path k)]]))
      m))

  (deftype Map
    [def
     ^:mutable val
     ^:mutable subnodes
     node-meta]
    INode
    (-recompute [this]
      ;(js/console.log "RECOMPUTE start" val)
      (if (nil? val)
        (do
          (set! subnodes (into {} (traverse-incr-map [] def)))
          (set! val
                (reduce (fn [m [node path]]
                          (assoc-in m path (exception-as-value #(do @node))))
                        def
                        subnodes))
          true)
        (doseq [s (.-dependencies node-meta)]
          ;(js/console.log "REC" s (.-change-time s) @current-time)
          (when (= @current-time (.-change-time s))
            (let [node (.-node s)]
              (set! val (assoc-in val (subnodes node) (exception-as-value #(do @node))))))))
      ;(js/console.log "RECOMPUTE end" val)
      true)
    (-get [this filter]
      (-subval-get val this filter))
    IDeref
    (-deref [this]
      (-get this nil))
    cljs.core/ILookup
    (-lookup [this k not-found] (or (-get this k) not-found))
    (-lookup [this k] (-get this k)))

  (defn imap [def]
    (Map. def nil nil (empty-node-meta 1)))

  (declare add-subscription!)

  ;; todo: gc
  ;; todo: notify only changed fields, changed? in recompute can return changed fields
  (defn getter [node filter]
    (if-let [t (get (.-getters node) filter)]
      t
      (let [t (thunk-f (fn []
                         (node-return-effects! node filter)
                         (get (.-val node) filter)))]
        (set! (.-getters node) (assoc (.-getters node) filter t))
        t)))

  (defn map-vals [node f]
    ; if gets value instead of node
    (if (implements? INode node)
      (thunk-f
        (fn []
          (let [prev-dep-val (.-prev-dep-val *parent-node*)
                curr-dep-val @node

                [old new _] (clojure.data/diff prev-dep-val curr-dep-val)
                val (.-val *parent-node*)
                val (reduce
                      (fn [val [k v]]
                        (assoc val k (f v)))
                      val
                      new)
                val (reduce
                      (fn [val [k _]]
                        (if (contains? new k)
                          val
                          (dissoc val k)))
                      val
                      old)]
            (set! (.-prev-dep-val *parent-node*) curr-dep-val)
            val)))
      (specter/transform [specter/MAP-VALS] f node)))

  (defn remove-subscription! [subscription]
    (js/console.log "remove-subscription!" subscription)
    (let [node (.-node subscription)
          observer (.-observer subscription)
          observer-meta (.-node-meta observer)
          node-meta (.-node-meta node)]
      (set! (.-subscriptions node-meta) (disj (.-subscriptions node-meta) subscription))
      (set! (.-dependencies observer-meta) (disj (.-dependencies observer-meta) subscription))
      (when (and (empty? (.-subscriptions node-meta))
                 (not (.-force-active node-meta)))
        (doseq [s (.-dependencies node-meta)] (remove-subscription! s)))))

  (defn add-subscription! [subscription]
    (let [node (.-node subscription)
          observer (.-observer subscription)
          observer-meta (.-node-meta observer)
          node-meta (.-node-meta node)
          was-empty? (and (empty? (.-subscriptions node-meta))
                          (not (.-force-active node-meta)))
          may-be-dirty? (and was-empty?
                             (not (implements? ICell node)))  ;; xxx: maybe cell can run without diffing when not observed
          ]
      (when-not (contains? (.-dependencies observer-meta)
                           subscription)
        (js/console.log "add-subscription!" (.-id node) (.-id observer) subscription)
        ;(js/console.log "add-subscription! node"  (count (.-subscriptions node-meta)) (count (.-dependencies node-meta)))
        ;(js/console.log "add-subscription! observer"  (count (.-subscriptions observer-meta)) (count (.-dependencies observer-meta)))
        (set! (.-subscriptions node-meta) (conj (.-subscriptions node-meta) subscription))
        (set! (.-dependencies observer-meta) (conj (.-dependencies observer-meta) subscription))
        (when may-be-dirty?
          ;(assert (< (.-height node-meta) (.-height observer-meta)))
          (to-dirty-nodes! [node])))
      may-be-dirty?))

  (defn node-return-effects! [node filter]
    (js/console.log "node-return-effects!" node filter)
    (when *parent-node*
      (let [sub (Subscription. node filter *parent-node* @current-time)
            may-be-dirty? (add-subscription! sub)]

        (when (and *height-limit*
                   (<= *height-limit* (.-height (.-node-meta node))))
          (println "OutOfHeight." (.-height (.-node-meta node)) ">=" *height-limit*)
          ;; todo: set height and go on if it is first run (if no one higher depends on this)
          ;; return value and height?
          (throw (OutOfHeight. (.-height (.-node-meta node)))))

        (when *referred-subscriptions*
          (vswap! *referred-subscriptions* conj sub))

        (when (and may-be-dirty?)
          ;(js/console.log "RS" this *parent-node*)
          ;(js/console.trace)
          (println "RestartStabilization.")
          ;; stabilize only to some height?
          (throw (RestartStabilization.))
          ))))

  (extend-protocol ISubVal
    #_(comment
        Cell
        (-subval-get [this node filter]
                     (node-return-effects! node nil #_filter)
                     (-subval-get @this this filter))
        Thunk
        (-subval-get [this node filter]
                     (node-return-effects! node nil #_filter)
                     (-subval-get @this this filter))
        Map
        (-subval-get [this node filter]
                     (node-return-effects! node filter)
                     (-get this)
                     ;(-subval-get @this this filter)
                     ))

    nil
    (-subval-get [this node filter]
      (node-return-effects! node filter)
      this)
    number
    (-subval-get [this node filter]
      (node-return-effects! node filter)
      this)
    boolean
    (-subval-get [this node filter]
      (node-return-effects! node filter)
      this)
    object
    (-subval-get [this node filter]
      (node-return-effects! node filter)
      (if (nil? filter)
        this
        (get this filter)))

    PersistentArrayMap
    (-subval-get [this node filter]
      (if (nil? filter)
        (do
          (node-return-effects! node filter)
          this)
        (getter node filter))
      )



    )

  ;; ISubVal / returns for thunks?


  (defn cell-set! [c v]
    (when (changed? (.-val c) v)
      (set! (.-val c) v)
      (to-dirty-nodes! [c])
      ))


  (defn assert-min-height! [h nodes]
    (doseq [node nodes]
      (when (> h (.-height (.-node-meta node)))
        (set! (.-height (.-node-meta node)) h))))

  (defn recompute-thunk! [t]
    (let [m (.-node-meta t)]
      (js/console.log "recompute-thunk!" t)

      (when (or (not (empty? (.-subscriptions m)))
                (.-force-active m))
        (try
          (binding [*height-limit* (.-height m)
                    *referred-subscriptions* (volatile! #{})
                    *parent-node* t]
            (let [changed? (-recompute t)
                  observers (map #(.-observer %) (.-subscriptions m))]

              (doseq [s (.-subscriptions m)]
                (set! (.-change-time s) @current-time))

              (js/console.log "after recompute" changed? (.-val t))

              (when changed?
                (assert-min-height! (inc (.-height m)) observers)
                (to-dirty-nodes! observers))
              (let [old-subs (.-dependencies m)
                    new-subs @*referred-subscriptions*
                    removed (clojure.set/difference old-subs new-subs)]
                (doseq [s removed] (remove-subscription! s))
                ;(set! (.-children t) new-children)
                )))
          (catch OutOfHeight e
            (set! (.-height m) (inc (.-h e)))
            (to-dirty-nodes! [t]))))))

  (defn stabilize-to-height! [h]
    (loop [i 0]
      (println "stabilize " i (aget dirty-nodes i))
      (while (not (empty? (aget dirty-nodes i)))
        (let [t (first (aget dirty-nodes i))]
          (recompute-thunk! t)
          (aset dirty-nodes i (disj (aget dirty-nodes i) t))))
      (when (and (< i h)
                 (< i (.-length dirty-nodes)))
        (recur (inc i)))))

  (defn stabilize* []
    (try
      (stabilize-to-height! *height-limit*)
      (catch RestartStabilization e
        (println "RestartStabilization" e)
        (stabilize*))))

  (defn stabilize! []
    (stabilize*)
    (swap! current-time inc))

  (defn set-active! [n]
    (set! (.-force-active (.-node-meta n)) true)
    (to-dirty-nodes! [n]))

  )