(ns ui-des.computed
  (:require [smidjen.core :refer-macros [facts fact]]
            clojure.set))


;; layers,
;; optimistic updates
;; historia dla czesci stanu
;; watch
;;

;; transakcje z metadanymi ?
;; - czy do historii
;; - czy to optimistic update

;; optimistic-updates (ze stanem aplikacji, typu okno z edycja zamknięte?)
;; app-state (z historia)
;; external-data

;; layer do navigatora


;; external-data -> history1 -> optimistic-update1 -> history2

(defn value [x]
  (if (implements? IDeref x)
    @x
    x))

;
;(defprotocol ISignal
;  (-value [this])
;  (add-subscriber! [this s])
;  (remove-subscriber! [this s]))
;
;(def ^:dynamic *refered-signals* nil)
;
;(defn return-signal-value [sig val]
;  (when *refered-signals*
;    (swap! *refered-signals* assoc sig val))
;  val)
;
;
;(deftype AtomSource [a subscribers]
;  ISignal
;  (-value [this]
;    (return-signal-value this @a))
;  (add-subscriber! [this s]
;    ;(swap! subscribers conj s)
;    )
;  (remove-subscriber! [this s]
;    ;(swap! subscribers disj s)
;    )
;  IDeref
;  (-deref [this] (-value this)))
;
;(defn atom-source [a]
;  (AtomSource. a (atom #{})))
;
;
;(defn- block-recalc [signal deps-vals f]
;  (binding [*refered-signals* (atom {})]
;    (let [res (f)
;          old (set (keys @deps-vals))
;          new (set (keys @*refered-signals*))
;          old-deps (clojure.set/difference old new)
;          new-deps (clojure.set/difference new old)]
;      ;(js/console.log "old" old-deps "new" new-deps)
;
;      (doseq [d old-deps] (when (implements? ISignal d) (remove-subscriber! d signal)))
;      (doseq [d new-deps] (when (implements? ISignal d) (add-subscriber! d signal)))
;
;      (reset! deps-vals @*refered-signals*)
;      res)))
;
;(declare block)
;
;(deftype Block [deps-vals val f subscribers impure]
;  ISignal
;  (-value [this]
;    (let [deps-changed (or impure
;                           (nil? @deps-vals)
;                           (some
;                             (fn [[dep prev-val]]
;                               (not= (value dep) prev-val))
;                             @deps-vals))]
;      (if deps-changed
;        (let [v (block-recalc this deps-vals f)]
;          (reset! val v)
;          (return-signal-value this v))
;        (return-signal-value this @val))))
;
;  (add-subscriber! [this s]
;    (swap! subscribers conj s))
;  (remove-subscriber! [this s]
;    ;; stop when 0 subs(?)
;    (swap! subscribers disj s))
;
;  IDeref
;  (-deref [this] (-value this))
;
;  ILookup  ; hack, use some kind of INavigator in store
;  (-lookup [this key]
;    (get (-value this) key))
;  )
;
;(defn block [f & [impure]]
;  (Block. (atom nil)
;          (atom nil)
;          f
;          (atom #{})
;          impure))

(defprotocol IObservable
  (-value [this])
  (add-subscriber! [this s])
  (remove-subscriber! [this s]))

(defprotocol IComputed
  (notify-stale [this s])
  (notify-ready [this s])
  (notify-ready-unchanged [this s]))


(defn observable-add-subscriber! [obs s]
  (vswap! (.-subscribers obs) conj s)
  (when (= 1 (count @(.-subscribers obs)))
    (when-let [f (.-on-start obs)] (f))))

(defn observable-remove-subscriber! [obs s]
  (vswap! (.-subscribers obs) disj s)
  (when (zero? (count @(.-subscribers obs)))
    (when-let [f (.-on-stop obs)] (f))))


(def ^:dynamic *refered-observable* nil)

(defn return-observable-value [sig val]
  ;(println "rov" val)
  (when *refered-observable*
    (vswap! *refered-observable* conj sig))
  val)

(defn- compute-recalc [cmp]
  (binding [*refered-observable* (volatile! #{})]
    (let [f (.-compute-fn cmp)
          subscriptions (.-subscriptions cmp)
          res (f)
          old (set @subscriptions)
          new (set @*refered-observable*)
          old-deps (clojure.set/difference old new)
          new-deps (clojure.set/difference new old)]
      ;(js/console.log "old" old-deps "new" new-deps)
      ;(println "old" old-deps "new" new-deps)
      (doseq [d old-deps] (remove-subscriber! d cmp))
      (doseq [d new-deps] (add-subscriber! d cmp))
      (vreset! subscriptions @*refered-observable*)
      res)))

(deftype Observable
  [subscribers
   val
   on-start
   on-stop]
  IObservable
  (-value [this] (return-observable-value this @val))
  (add-subscriber! [this s] (observable-add-subscriber! this s))
  (remove-subscriber! [this s] (observable-remove-subscriber! this s))
  IDeref
  (-deref [this] (-value this)))

(defn computed-notify-subs [cmp signal]
  (doseq [sub @(.-subscribers cmp)]
    (signal sub cmp)))

(defn computed-notified-ready [cmp s]
  (vreset! (.-stales cmp) (disj @(.-stales cmp) s))
  (when (empty? @(.-stales cmp))
    (if-not @(.-deps-changed? cmp)
      (computed-notify-subs cmp notify-ready-unchanged)
      (let [new-value (compute-recalc cmp)]
        (if (= new-value @(.-last-val cmp))  ;; deep eq / hash
          (computed-notify-subs cmp notify-ready-unchanged)
          (do
            (vreset! (.-last-val cmp) new-value)
            (computed-notify-subs cmp notify-ready)))))
    (vreset! (.-deps-changed? cmp) false)))

(deftype IComputed
  [subscribers
   subscriptions
   last-val
   compute-fn
   stales
   deps-changed?]
  IObservable
  (-value [this]
    (return-observable-value
      this
      (if (pos? (count @subscribers))
        @last-val
        (if *refered-observable*
          (vreset! last-val (compute-recalc this))
          (vreset! last-val (compute-fn))))))
  (add-subscriber! [this s] (observable-add-subscriber! this s))
  (remove-subscriber! [this s] (observable-remove-subscriber! this s))
  IComputed
  (notify-stale [this s]
    (when (empty? @stales)
      (computed-notify-subs this notify-stale))
    (vreset! stales (conj @stales s)))
  (notify-ready [this s]
    (vreset! deps-changed? true)
    (computed-notified-ready this s))
  (notify-ready-unchanged [this s]
    (computed-notified-ready this s))
  IDeref
  (-deref [this] (-value this)))

(def ^:dynamic *trx* nil)

(defn in-trx [f]
  (binding [*trx* (volatile! #{})]
    (f)
    (doseq [observable @*trx*]
      (computed-notify-subs observable notify-ready))))

(defn observalbe-set! [observable val]
  (when (not= val @(.-val observable))
    (computed-notify-subs observable notify-stale)
    (vreset! (.-val observable) val)
    (if *trx*
      (vswap! *trx* conj observable)
      (computed-notify-subs observable notify-ready))))

(defn observable
  "Returns new val fn"
  [val on-start on-stop]
  (Observable.
    (volatile! #{})
    (volatile! val)
    on-start
    on-stop))

(defn computed [f]
  (IComputed.
    (volatile! #{})
    (volatile! #{})
    (volatile! nil)
    f
    (volatile! #{})
    (volatile! false)))

(def bottom-computed
  (IComputed.
    (volatile! #{})
    (volatile! #{})
    (volatile! nil)
    (fn [])
    (volatile! #{})
    (volatile! false)))

(defn reaction [f]
  (let [r (IComputed.
            (volatile! #{bottom-computed})
            (volatile! #{})
            (volatile! nil)
            (fn [] (f) nil)
            (volatile! #{})
            (volatile! true))]
    (computed-notified-ready r nil)
    r))

(comment
  (let [o1 (observable 1 (fn []) (fn []))
        o2 (observable 1 (fn []) (fn []))
        c1 (computed (fn []
                       (println "CCC")
                       (+ @o1 @o2)))
        r1 (reaction (fn [] (println "R1" @c1)))
        ]
    ;@c1
    (in-trx
      (fn []
        (observalbe-set! o1 2)
        (observalbe-set! o2 2)))
    ;(.-subscribers o1)
    ;@r1
    )


  )


(facts "blocks"



  #_(let [a (atom 1)
        b (atom 1)
        s (atom-source a)
        s1 (atom-source b)
        comp-counter (atom 0)
        x (block (fn []
                   (swap! comp-counter inc)
                   (+ @s @s1)))]
    @comp-counter => 0
    @x => 2
    @comp-counter => 1
    @x => 2
    @comp-counter => 1
    (reset! a 2)
    @x => 3
    (reset! b 2)
    @comp-counter => 2
    @x => 4
    @comp-counter => 3
    )
  #_(let [x (atom :x)
        y (atom :y)
        b (atom true)
        sx (atom-source x)
        sy (atom-source y)
        sb (atom-source b)
        comp-counter (atom 0)
        r (block (fn []
                   (swap! comp-counter inc)
                   (if @sb @sx @sy)))]
    @comp-counter => 0
    @r => :x
    @comp-counter => 1
    (reset! y :yy)
    @r => :x
    @comp-counter => 1
    (reset! b false)
    @r => :yy
    @comp-counter => 2

   ))

