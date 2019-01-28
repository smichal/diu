(ns ui-des.signals
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


(defprotocol ISignal
  (-value [this])
  (add-subscriber! [this s])
  (remove-subscriber! [this s]))

(def ^:dynamic *refered-signals* nil)

(defn return-signal-value [sig val]
  (when *refered-signals*
    (swap! *refered-signals* assoc sig val))
  val)

(defn value [x]
  (if (implements? IDeref x)
    @x
    x))

(deftype AtomSource [a subscribers]
  ISignal
  (-value [this]
    (return-signal-value this @a))
  (add-subscriber! [this s]
    ;(swap! subscribers conj s)
    )
  (remove-subscriber! [this s]
    ;(swap! subscribers disj s)
    )
  IDeref
  (-deref [this] (-value this)))

(defn atom-source [a]
  (AtomSource. a (atom #{})))


(defn- block-recalc [signal deps-vals f]
  (binding [*refered-signals* (atom {})]
    (let [res (f)
          old (set (keys @deps-vals))
          new (set (keys @*refered-signals*))
          old-deps (clojure.set/difference old new)
          new-deps (clojure.set/difference new old)]
      ;(js/console.log "old" old-deps "new" new-deps)

      (doseq [d old-deps] (when (implements? ISignal d) (remove-subscriber! d signal)))
      (doseq [d new-deps] (when (implements? ISignal d) (add-subscriber! d signal)))

      (reset! deps-vals @*refered-signals*)
      res)))

(declare block)

(deftype Block [deps-vals val f subscribers impure]
  ISignal
  (-value [this]
    (let [deps-changed (or impure
                           (nil? @deps-vals)
                           (some
                             (fn [[dep prev-val]]
                               (not= (value dep) prev-val))
                             @deps-vals))]
      (if deps-changed
        (let [v (block-recalc this deps-vals f)]
          (reset! val v)
          (return-signal-value this v))
        (return-signal-value this @val))))

  (add-subscriber! [this s]
    (swap! subscribers conj s))
  (remove-subscriber! [this s]
    ;; stop when 0 subs(?)
    (swap! subscribers disj s))

  IDeref
  (-deref [this] (-value this))

  ILookup  ; hack, use some kind of INavigator in store
  (-lookup [this key]
    (get (-value this) key))
  )

(defn block [f & [impure]]
  (Block. (atom nil)
          (atom nil)
          f
          (atom #{})
          impure))

(facts "blocks"
  (let [a (atom 1)
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
  (let [x (atom :x)
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

