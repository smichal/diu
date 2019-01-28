(ns core.computed
  (:require clojure.set
            [cljs.test :refer-macros [deftest is testing run-tests]]))



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
    (when-let [f (.-on-start obs)] (f obs))))

(defn observable-remove-subscriber! [obs s]
  (vswap! (.-subscribers obs) disj s)
  (when (zero? (count @(.-subscribers obs)))
    (when-let [f (.-on-stop obs)] (f obs))
    (doseq [d (some-> (.-subscriptions obs) deref)]
      (remove-subscriber! d obs))))


(def ^:dynamic *refered-observable* nil)

(defn return-observable-value [sig val]
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
   on-stop
   current-value-fn]
  IObservable
  (-value [this] (return-observable-value this
                                          (if (or (pos? (count @subscribers))
                                                  (nil? current-value-fn))
                                            @val
                                            (current-value-fn))))
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
  (-deref [this]
    (-value this)))

(def ^:dynamic *trx* nil)

(defn in-trx [f]
  (binding [*trx* (volatile! #{})]
    (f)
    (doseq [observable @*trx*]
      (computed-notify-subs observable notify-ready))))

(defn observable-set! [observable val]
  (when (not= val @(.-val observable))
    (computed-notify-subs observable notify-stale)
    (vreset! (.-val observable) val)
    (if *trx*
      (vswap! *trx* conj observable)
      (computed-notify-subs observable notify-ready))))

(defn observable
  "Returns new val fn"
  [val on-start on-stop & [current-val-fn]]
  (Observable.
    (volatile! #{})
    (volatile! val)
    on-start
    on-stop
    current-val-fn))

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

(defn stop-reaction [r]
  (remove-subscriber! r bottom-computed))

(deftest test1
  (let [o1 (observable 1 (fn []) (fn []))
        o2 (observable 1 (fn []) (fn []))
        cmp-count (atom 0)
        reactions (atom [])
        c1 (computed (fn []
                       (swap! cmp-count inc)
                       (+ @o1 @o2)))
        r1 (reaction (fn [] (swap! reactions conj @c1)))
        ]
    (is (= @c1 2))
    (is (= @cmp-count 1))

    (in-trx
      (fn []
        (observable-set! o1 2)
        (observable-set! o2 2)))
    (is (= @c1 4))
    (is (= @cmp-count 2))
    (is (= @reactions [2 4]))
    ))