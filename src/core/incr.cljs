(ns core.incr
  (:require clojure.set
            [cljs.test :as test :refer-macros [deftest is testing run-tests]]
            [clojure.test.check.generators :as gen]))


(defprotocol INode
  (-value [this])
  (-height [this])
  ;(remove-subscriber! [this s])
  ;(remove-subscriber! [this s])
  )


(defprotocol ICell)

(deftype OutOfHeight [h])
(deftype RestartStabilization [])

(def ^:dynamic *height-limit* 100000)
(def ^:dynamic *referred-nodes* nil)
(def ^:dynamic *parent-node* nil)
(def ^:dynamic *diff-from* 0)

(declare add-parent!)

(declare current-time)

(defn node-return-effects! [this]
  (when (and *height-limit*
             (<= *height-limit* (.-height this)))
    ;(println "OutOfHeight." (.-height this) ">=" *height-limit*)
    ;; todo: set height and go on if it is first run (if no one higher depends on this)
    ;; return value and height?
    (throw (OutOfHeight. (.-height this))))

  (when *parent-node*
    (when (add-parent! this *parent-node*)
      ;(js/console.log "RS" this *parent-node*)
      ;(js/console.trace)
      ;(println "RestartStabilization." (.-recompute-time this) " at " @current-time)
      ;; stabilize only to some height?
      (throw (RestartStabilization.))))

  (when *referred-nodes*
    (vswap! *referred-nodes* conj this)))

(declare iget)


(deftype Cell [^:mutable val
               ^:mutable parents
               ^:mutable change-time]
  ICell
  INode
  (-value [this]
    (node-return-effects! this)
    val)
  (-height [_] 0)
  IDeref
  (-deref [this] (-value this))

  cljs.core/ILookup
  (-lookup [this k not-found] (iget this k not-found))
  (-lookup [this k] (-lookup this k nil))
  )

(defprotocol IDiffNode
  (-diff-from [this time])
  )

(defn cell [v]
  (Cell. v #{} -1))

(def dirty-nodes #js [])
(def current-time (atom 1))

(defprotocol IDiffPatch
  (-diff [this other])
  (-patch [this diff])
  (-reduce-diff [value diff])
  )

(extend-type default
  IDiffPatch
  (-patch [a b] b)
  (-reduce-diff [value diff] diff)
  (-diff [a b] b))

(defn deleted? [x]
  (= ::deleted x))

#_(defn or-deleted [x v]
  (if (= ::deleted x) x v))

(extend-type PersistentArrayMap
  IDiffPatch
  (-patch [v d]
    (if (= d ::deleted)
      nil
      (reduce (fn [m k]
                (if (= (get d k) ::deleted)
                  (dissoc m k)
                  m))
              (merge-with -patch v d)
              (keys d))))
  (-reduce-diff [value diff]
    ;; fixme, performance, nested vals
    ;(second (clojure.data/diff value diff))
    (->> diff
         (remove (fn [[k v]]
                   (or (= v (get value k))
                       (= v {})
                       )))
         (into {})))
  (-diff [a b]
    (if (::diff (meta b))
      b
      (with-meta
        (let [keys-a (set (keys a))
              keys-b (set (keys b))
              deleted (clojure.set/difference keys-a keys-b)
              new (clojure.set/difference keys-b keys-a)
              common (clojure.set/intersection keys-a keys-b)]
          (merge
            (->> deleted (map #(do [% ::deleted])) (into {}))
            (->> new (map #(do [% (b %)])) (into {}))
            (->> common (keep #(let [av (a %)
                                     bv (b %)]
                                 (if (and (not= av bv)
                                          (not (and (= bv {})
                                                    (::diff (meta bv)))))
                                   (let [diff (-diff av bv)]
                                     ;(println "diff" av bv diff)
                                     (when-not (= {} diff) #_(empty? diff) ; redundant?
                                       [% diff])))))
                 (into {}))))
        {::diff true}))))

(defn merge-diffs [ds]
  (reduce -patch ds))

(defn mark-as-diff [v]
  (if (implements? IDiffPatch v)
    (with-meta v {::diff true})
    v))

(deftype DiffCell [^:mutable diffs
                   ^:mutable parents
                   ^:mutable change-time]
  ICell
  INode
  (-value [this]
    (node-return-effects! this)
    (-diff-from this 0))

  (-height [_] 0)

  IDiffNode
  (-diff-from [this time]
    (node-return-effects! this)
    (mark-as-diff (or (merge-diffs (vals (subseq diffs >= time))) #_{}))
    #_(with-meta
      (merge-diffs (vals (subseq diffs >= time)))
      {::diff true}))

  IDeref
  (-deref [this]
    (-diff-from this *diff-from*)))

(defn diff-cell [v]
  (DiffCell.
    (sorted-map 0 v)
    #{}
    -1))

(defn changed? [old new]
  (not= old new))


(defn to-dirty-nodes! [nodes]
  (doseq [node nodes]
    (let [h (-height node)]
      ;(println "to-dirty" h node)
      (if-let [s (aget dirty-nodes h)]
        (aset dirty-nodes h (conj s node))
        (aset dirty-nodes h #{node})))))

(defn cell-set! [c v]
  (when (changed? (.-val c) v)
    ;(swap! changed-cells conj c)
    (set! (.-val c) v)
    (set! (.-change-time c) @current-time)
    (to-dirty-nodes! (.-parents c))
    ))

(defn cell-swap! [c f]
  (cell-set! c (f (.-val c))))

(defn path-diff-cell! [c d]
  (set! (.-diffs c)
        (update (.-diffs c)
                @current-time
                #(merge-diffs [% d])))

  (set! (.-change-time c) @current-time)
  (to-dirty-nodes! (.-parents c)))

(declare stabilize-to-height!)

(defn remove-parent! [node parent]
  (set! (.-parents node) (disj (.-parents node) parent))
  (when (empty? (.-parents node))
    (doseq [n (.-children node)] (remove-parent! n node))))


(defn add-parent! [node parent]
  (let [was-empty? (empty? (.-parents node))
        may-be-dirty? (and was-empty?
                           (not (implements? ICell node)))]
    (when-not (contains? (.-parents node) parent)
      (set! (.-parents node) (conj (.-parents node) parent))
      (when may-be-dirty?
        ;(println "add-parent" (-height node) (-height parent))
        (assert (< (-height node) (-height parent)))
        (to-dirty-nodes! [node])
        (doseq [n (.-children node)] (add-parent! n node))))
    may-be-dirty?))

(defn call-sub-thunk [key f & args]
  (assert *parent-node*)
  (let [[thunk prev-f prev-args _] (get (.-subcomputations *parent-node*) key)
        new-thunk (when (or (not thunk)
                            (not= prev-f f)
                            (not= prev-args args))
                    ;(js/console.log "XXX" (not= prev-f f) (not= prev-args args))
                    ;(js/console.log "OLD THUNK" prev-f prev-args)
                    ;(js/console.log "NEW THUNK" f args)
                    (apply f args))
        thunk (or new-thunk thunk)]
    (set! (.-subcomputations *parent-node*)
          (assoc (.-subcomputations *parent-node*) key [thunk f args @current-time]))
    thunk))

(defn log [& args]
  (apply js/console.log args))
(defn spy [label e]
  (log label "=>" e)
  e)

(defprotocol IThunk
  (-recompute [this]))

(deftype Thunk [f
                ^:mutable val
                ^:mutable parents
                ^:mutable children
                ^:mutable height
                ^:mutable recompute-time
                ^:mutable change-time
                ^:mutable subcomputations]
  INode
  (-height [_] height)
  (-value [this]
    (node-return-effects! this)
    val)
  IThunk
  (-recompute [this]
    (let [new-val (f)
          changed (changed? val new-val)]
      (set! val new-val)
      changed))
  IDeref
  (-deref [this] (-value this))

  cljs.core/ILookup
  (-lookup [this k not-found] (iget this k not-found))
  (-lookup [this k] (iget this k))
  )

(deftype DiffThunk [f-diff
                    f-val
                    ^:mutable diffs
                    ^:mutable value
                    ^:mutable parents
                    ^:mutable children
                    ^:mutable height
                    ^:mutable recompute-time
                    ^:mutable change-time
                    ^:mutable subcomputations]
  INode
  (-height [_] height)
  (-value [this]
    (node-return-effects! this)
    value
    ;(-diff-from this 0)
    )

  IDiffNode
  (-diff-from [this time]
    (node-return-effects! this)
    ;(log "-diff-from" time (subseq diffs >= time))

    (mark-as-diff (or (merge-diffs (vals (subseq diffs >= time))) {}))

    #_(with-meta
      (or (merge-diffs (vals (subseq diffs >= time))) {})
      ;; not ^:diff for first (whole) value,
      ;; could help when one DiffThunk changes to another (and both are part of map)
      ;(when (< 1 (count diffs)) {::diff true})
      {::diff true}))

  IThunk
  (-recompute [this]
    ;(js/console.log "recompute-thunk! DIFF" this)
    ;; fixme:  what if there are skipped changes before `recompute-time`?
    ;; remember last fetch time for each child?
    (binding [*diff-from* (inc recompute-time)]
      (let [new-diff (if f-diff
                       (-reduce-diff value (f-diff))
                       (let [v (f-val)]
                         #_(println "-diff" (pr-str value)
                                  (pr-str v)
                                  (meta v)
                                  "=>" (-diff value v))
                         (-diff value v)))
            ;new-diff (-reduce-diff value new-diff)
            ]
        ;(println "new-diff" value (f-val) "=>" new-diff)
        (when new-diff
          (set! value (-patch value new-diff))
          (set! diffs (assoc diffs @current-time new-diff)))
        (not (nil? new-diff)))))

  IDeref
  (-deref [this]
    (-diff-from this *diff-from*))
  )

(defn diff-thunk
  ([v0 f] (DiffThunk. f nil (sorted-map 0 v0) v0 #{} #{} 1 -1 -1 nil))
  ([f] (DiffThunk. f nil (sorted-map) nil #{} #{} 1 -1 -1 nil)))

(defn diff-val-thunk
  ([v0 f] (DiffThunk. nil f (sorted-map 0 v0) v0 #{} #{} 1 -1 -1 nil))
  ([f] (DiffThunk. nil f (sorted-map) nil #{} #{} 1 -1 -1 nil)))

(defn map-kv [im f ]
  ; f(k, iv)
  (diff-thunk
    (fn []
      ;(log "MAP KV" *diff-from* @im)
      (into {}
        (map (fn [[k v]]
               (f k v))
             @im)))))

(deftype Observer []
  INode
  (-height [_] 10)                                          ; fixme
  (-value [_] nil)

  )

(defn assert-min-height! [h nodes]
  (doseq [node nodes]
    (when (> h (.-height node))
      (set! (.-height node) h))))

(defn recompute-thunk! [t]

  (when (implements? IThunk t)

    ;(js/console.log "recompute-thunk!" t)

    (assert (.-parents t) "recompute only if has parents")
    ;(assert (< (.-recompute-time t) @current-time) "never recompute twice in one cycle")

    (when (or (neg? (.-recompute-time t))
              (some #(> (.-change-time %) (.-recompute-time t))
                    (.-children t)))
      (try
        (binding [*height-limit* (-height t)
                  *referred-nodes* (volatile! #{})
                  *parent-node* t]
          (let [changed? (-recompute t)]
            (set! (.-recompute-time t) @current-time)
            (when changed?
              (set! (.-change-time t) @current-time)
              (assert-min-height! (inc (.-height t)) (.-parents t))
              (to-dirty-nodes! (.-parents t)))
            (let [old-children (.-children t)
                  new-children @*referred-nodes*
                  removed (clojure.set/difference old-children new-children)]
              (doseq [n removed] (remove-parent! n t))
              (set! (.-children t) new-children))

            ;; gc subcomputations
            (set! (.-subcomputations t)
                  (->> (.-subcomputations t)
                       (filter (fn [[_ [_ _ _ time]]] (= time @current-time)))
                       (into {})))
            ))
        (catch OutOfHeight e
          ;(println "OutOfHeight" (-height t) "=>" (inc (.-h e)))
          (set! (.-height t) (inc (.-h e)))
          (to-dirty-nodes! [t])
          ))
      )))

(defn thunk [f]
  (Thunk. f nil #{} nil 1 -1 -1 nil))

(defn stabilize-to-height! [h]
  (loop [i 1]
    ;(println "stabilize " i (aget dirty-nodes i))
    (while (not (empty? (aget dirty-nodes i)))
      (let [t (first (aget dirty-nodes i))]
        (recompute-thunk! t)
        (aset dirty-nodes i (disj (aget dirty-nodes i) t))))
    (when (and (< i h)
               (< i (.-length dirty-nodes)))
      (recur (inc i)))
    ))

(defn stabilize* []
  (try
    (stabilize-to-height! 1000000)
    (catch RestartStabilization e
      (stabilize*))))

(defn stabilize! []
  ;(log "stabilize!" dirty-nodes)
  (stabilize*)
  #_(try
    (stabilize-to-height! 1000000)
    (catch RestartStabilization e
      (stabilize!)))
  ;(log "after stabilize!" dirty-nodes)
  (swap! current-time inc))


(def scheduled? (atom false))
(defn schedule-stabilization! []
  (when-not @scheduled?
    (reset! scheduled? true)
    (js/setTimeout (fn []
                     (reset! scheduled? false)
                     (stabilize!))
                   1)))


;; depracated
;; used only in i-get
;; refactor, remove
(defn memo-named-thunk* [thunk-constructor f]
  (let [mem (atom {})
        memo-name-atom (atom (sorted-map))]
    [(fn [& args]
       (let [k (some-> args first meta :incr/name)
             [args-cell thunk] (if k (get @memo-name-atom k)
                                     (get @mem args))]
         ;(println "META :incr/name" (.-name f) (or (some-> args first meta :incr/name) args) thunk)
         (if (nil? thunk)
           (let [args-cell (cell args)
                 thunk (thunk-constructor (fn [] (apply f @args-cell)))]
             (if k
               (swap! memo-name-atom assoc k [args-cell thunk])
               (swap! mem assoc args [args-cell thunk]))
             thunk)
           (do
             (cell-set! args-cell args)
             ;(schedule-stabilization!)                      ;; fixme
             (when k
               (println "CHANGE " k (:props (first args))))
             thunk))))
     memo-name-atom
     ]))

(defn memo-named-thunk [thunk-constructor f]
  (first (memo-named-thunk* thunk-constructor f)))

(defn remove-memos-with-prefix [memo-name-atom prefix]
  ;(println "remove-memos-with-prefix" prefix)
  (let [ks (map first (subseq @memo-name-atom >= prefix <= (str prefix \uffff)))]
    (swap! memo-name-atom (fn [m] (apply dissoc m ks)))))

;; fixme: GC
;; val -> val
(defn memo* [f]
  (memo-named-thunk* thunk f))

(def memo
  (comp first memo*))

;; diff -> diff
(defn diff-memo [f]
  (memo-named-thunk diff-thunk f)
  #_(memoize
      (fn [& args]
        (diff-thunk (fn []
                      (apply f args))))))

;; val -> diff
(defn diff-val-memo [f]
  (memo-named-thunk diff-val-thunk f))

(defn diff-val-memo* [f]
  (memo-named-thunk* diff-val-thunk f))

(def iget
  (memo (fn m-iget [m & args]
          (apply get @m args))))


(defn fmap [f & args]
  (thunk (fn []
           (apply f (map deref args)))))

#_(defn -nodes-in-map [path m]
  (reduce-kv
    (fn [acc k v]
      (cond
        (implements? INode v) (conj acc [v (conj path k)])
        (map? v) (concat acc (-nodes-in-map (conj path k) v))
        :else acc))
    []
    m))

#_(def mapi
  (memoize
    (fn [m]
      (let [thunk->path (-nodes-in-map [] m)]
        (js/console.log "QQQQQ" thunk->path)
        (diff-thunk m
                    (fn []
                      (reduce
                        (fn [res [thunk path]]
                          (println "MAPi " path @thunk)
                          (if-let [diff @thunk]
                            (assoc-in res path diff)
                            res))
                        {}
                        thunk->path)))))))



#_(deftest cell-test
  (let [c (cell 1)]
    (is (= (-value c) 1))
    (cell-set! c 2)
    (is (= (-value c) 2))
    ))

#_(deftest thunk-test
  (let [a (cell 1)
        b (cell 1)
        c (cell true)
        t (thunk (fn []
                   (js/console.log "T thunk computing")
                   (+ @a @b)))
        f (thunk (fn []
                    (js/console.log "F thunk computing")
                    (if @c
                      @t
                      nil
                      )))]
    (add-parent! t (Observer.))
    (add-parent! f (Observer.))

    (js/console.log "1" t)
    (stabilize!)
    (js/console.log "2" t)
    (js/console.log dirty-nodes)
    (is (= (.-val t) 2))
    (cell-set! a 2)
    (cell-set! b 2)
    (is (= (.-val t) 2))
    (stabilize!)
    (js/console.log "3" t)
    (is (= (.-val t) 4))


    (is (= (.-val t) (.-val f)))
    (log "to false")
    (cell-set! c false)
    (stabilize!)
    (is (= nil (.-val f)))

    (log "to true")
    (cell-set! c true)
    (stabilize!)
    (is (= 4 (.-val f)))

    ))


#_(deftest diff-cell-test
  (let [c (diff-cell {:a 1})]
    (is (= (-value c) {:a 1}))
    (path-diff-cell! c {:b 2})
    (is (= (-value c) {:a 1 :b 2}))
    (path-diff-cell! c {:b 3})
    (is (= (-value c) {:a 1 :b 3}))


    (let [mc (map-kv c (fn [k v]
                         [k (inc v)]))]
      (add-parent! mc (Observer.))
      (stabilize!)
      (is (= @mc {:a 2 :b 4}))

      (path-diff-cell! c {:a 2})
      (stabilize!)
      (is (= @mc {:a 3 :b 4}))
      )

    )

  )


#_(deftest example1
  (log "Example1")
  (let [data {1 {:name "a" :count 2}
              2 {:name "b" :count 3}}
        c (diff-cell data)
        r-item (fn [k item]
                 [k {:tag :p :children {(+ 10 k) (str "name: " (:name item))}}])
        t2 (map-kv c r-item)

        t1 (diff-thunk
             {:tag :div :children {}}
             (fn []
               {                                       ;:tag :div
                :children @t2}))

        node (fn node [parent prefix x f]
               (into {}
                     (cons
                       (when-let [nd (f parent x)] [prefix nd])
                       (map (fn [[k v]]
                              (node prefix (str prefix "-" k) v f))
                            (:children x)))))

        ; tree-walk-node

        t0 (diff-thunk
             (fn []
               (node "root" "N" @t1
                     (fn [p x]
                       (cond
                         (string? x) {:text x}
                         (:tag x) {:tag (:tag x)}
                         )
                       #_{:tag :div
                                :parent p
                                :text (if (string? x) x)})
                     )
               ))
        ]
    (add-parent! t0 (Observer.))
    (stabilize!)
    (log @t0)
    (path-diff-cell! c {1 {:name "aa"}})
    (stabilize!)
    (log @t0)
    (log t0)
    (log t1)
    )
  )

#_(deftest bench1
  (let [n 1000
        m (zipmap (range n) (range n))
        state (atom m)

        c (diff-cell m)
        mc (map-kv c (fn [k v]
                       [k (inc v)]))
        ]

    (time
      (dotimes [i 1000]
        (swap! state assoc (rand-int n) (rand-int n))))
    (time
      (dotimes [i 1000]
        (path-diff-cell! c {(rand-int n) (rand-int n)})
        (stabilize!)
        ))

    )


  )

; cell val [on-start on-stop]
; thunk fn
; reaction fn
; @cell
; step!
; ; cell-get / map (get one, do for all, reduce)
;
;
;
;
;
;
