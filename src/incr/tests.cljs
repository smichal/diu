(ns incr.tests
  (:require
    [incr.core :as incr :include-macros true]
    [cljs.test :as test :refer-macros [deftest is testing run-tests]]))

(deftest thunk1
  (let [c (incr/cell {:a 1})
        f (fn [] (update @c :a inc))
        t (incr/incr f)
        t2 (incr/thunk (update @c :a inc))
        ]

    (incr/stabilize!)

    (js/console.log "C" c)
    (js/console.log "T" t)
    (js/console.log @incr/nodes)

    (is (= @c {:a 1}))
    (is (= @(incr/incr f) {:a 2}))

    (reset! c {:a 3})

    (incr/stabilize!)

    (is (= @c {:a 3}))

    (is (= @t2 {:a 4}))

    ))

(deftest thunk-dependencies2
  (let [c (incr/cell 1)
        t1-count (atom 0)
        t2-count (atom 0)
        t1 (incr/thunk
             (js/console.log "#### t1")
             (js/console.log "#### t1 REF" @c)
             (swap! t1-count inc)
             [@c @c])
        t2 (incr/thunk
             (js/console.log "#### t2")
             (swap! t2-count inc)
             {:c @c})
        t (incr/thunk
            (js/console.log "#### t3")
            (if (< @c 5) @t1 @t2))]
    (js/console.log "thunk-dependencies2")

    (incr/stabilize!)

    (is (= @t [1 1]))
    (is (= 1 @t1-count))
    (is (= 0 @t2-count))

    (reset! c 2)
    (incr/stabilize!)

    (is (= @t [2 2]))
    (is (= 2 @t1-count))

    (js/console.log "Cell set 8")
    (reset! c 8)
    (incr/stabilize!)

    (is (= @t {:c 8}))
    ))

(deftest subdependecies1
  (js/console.log "subdependecies1")
  (let [c (incr/cell {:a 1
                      :b 1
                      :c 1})
        t-count (atom 0)

        t (incr/thunk
             (swap! t-count inc)
             (inc @(incr/incr get c :a)))]

    (incr/stabilize!)
    (is (= @t 2))
    (is (= 1 @t-count))

    ;(js/console.log @incr/nodes)
    (reset! c {:a 1
               :b 2
               :c 2})
    (incr/stabilize!)
    (is (= @t 2))
    (is (= 1 @t-count))
    ))

(deftest incr-map
  (reset! incr/nodes {})

  (js/console.log "incr-map")
  (let [c1 (incr/cell 1)
        c2 (incr/cell 10)
        t2-count (atom 0)
        c-count (atom 0)
        t2 (incr/thunk
             (swap! t2-count inc)
             @c2)
        m (incr/thunk {:a @c1
                       :b @t2
                       :c @(incr/thunk
                             (swap! c-count inc)
                             (inc @c2))})]

    (incr/stabilize!)
    (is (= {:a 1 :b 10 :c 11} @m))
    (is (= 1 @t2-count))
    (is (= 1 @c-count))

    (js/console.log @incr/nodes)

    (reset! c1 2)
    (incr/stabilize!)
    (is (= {:a 2 :b 10 :c 11} @m))
    (is (= 1 @t2-count))
    (is (= 1 @c-count))
    ))


(deftype TestException [])
(set! (.-prototype TestException) (js/Error.))

(deftest exceptions
  (let [ex (TestException.)
        t1 (incr/thunk
             (throw ex))
        t2 (incr/thunk (inc @t1))
        m (incr/thunk {:a (incr/try-deref t1)})]

    (is (thrown? TestException @t2))
    (incr/stabilize!)
    (is (thrown? TestException @t2))
    (is (= @m {:a ex}))
    ))

(deftest imap
  (is (implements? incr/IIncr map))
  (is (= [2 3 4] @(incr/incr map inc [1 2 3])))
  (let [f (fn [x]
            (println "INC " x)
            (inc x))
        c (incr/cell [1 2 3])
        t (incr/incr map f c)]

    (is (= [2 3 4] @t))
    (incr/stabilize!)
    (reset! c [5 2 3])
    (incr/stabilize!)
    (is (= [6 3 4] @t))

    ))

(deftest gctest
  (reset! incr/nodes {})
  (let [a (incr/cell true)
        b (incr/thunk 42)
        c (incr/thunk (if @a @b 0))
        ]
    @c
    (incr/stabilize!)
    (is (= (count @incr/nodes) 3))
    (reset! a false)
    (incr/stabilize!)
    (is (= (count @incr/nodes) 2))
    (js/console.log @incr/nodes)
    ))

(comment

#_(deftest cell
  (let [c (incr/cell {:a 1})]
    (is (= (incr/-get c nil) {:a 1}))
    (is (= (incr/-get c :a) 1))
    (incr/cell-set! c {:a 2})
    (is (= (incr/-get c :a) 2))
    ))

(deftest thunk1
  (let [c (incr/cell {:a 1})
        t (incr/thunk
            (update @c :a inc))]
    (incr/set-active! t)
    (incr/stabilize!)
    (is (= @t {:a 2}))))

(deftest thunk-dependencies1
  (let [c (incr/cell {:a 1})
        t (incr/thunk
            (update @c :a inc))]
    (incr/set-active! t)

    (incr/stabilize!)

    (js/console.log "t" t)
    (is (= (.-dependencies (.-node-meta t))
           #{(incr/Subscription. c nil t 0)}))

    (is (= @t {:a 2}))

    (js/console.log "Cell set")
    (incr/cell-set! c {:a 2})

    (incr/stabilize!)
    (is (= @t {:a 3}))))

(deftest subscription
  (let [c (incr/cell {:a 1})
        t (incr/thunk)]
    (is (= (incr/Subscription. c :a t 0) (incr/Subscription. c :a t 1)))
    (is (= 1 ({(incr/Subscription. c :a t 0) 1} (incr/Subscription. c :a t 1))))))

(deftest thunk-dependencies2
  (let [c (incr/cell 1)
        t1-count (atom 0)
        t2-count (atom 0)
        t1 (incr/thunk
             (swap! t1-count inc)
             [@c @c])
        t2 (incr/thunk
             (swap! t2-count inc)
             {:c @c})
        t (incr/thunk
            (if (< @c 5) @t1 @t2))]
    (incr/set-active! t)

    (js/console.log "thunk-dependencies2")

    (incr/stabilize!)

    (is (= @t [1 1]))
    (is (= 2 (count (.-dependencies (.-node-meta t)))))
    (is (= 1 @t1-count))
    (is (= 0 @t2-count))

    (incr/cell-set! c 2)
    (incr/stabilize!)

    (is (= @t [2 2]))
    (is (= 2 (count (.-dependencies (.-node-meta t)))))

    (js/console.log "Cell set 8")
    (incr/cell-set! c 8)
    (incr/stabilize!)

    (is (= @t {:c 8}))
    (is (= 2 (count (.-dependencies (.-node-meta t)))))))

(deftest subdependecies1
  (js/console.log "subdependecies1")
  (let [c (incr/cell {:a 1
                      :b 1
                      :c 1})
        t-count (atom 0)
        t (incr/thunk
            (swap! t-count inc)
            ;(js/console.log "XXX" (get c :a))
            (inc (incr/value (get c :a))))]
    (incr/set-active! t)
    (incr/stabilize!)
    (is (= @t 2))
    (is (= 2 @t-count))

    (reset! c {:a 1
               :b 2
               :c 2})
    (incr/stabilize!)
    (is (= 2 @t-count))))

(deftest incr-map
  (js/console.log "incr-map")
  (let [c1 (incr/cell 1)
        c2 (incr/cell 2)
        t2-count (atom 0)
        t2 (incr/thunk
             (swap! t2-count inc)
             @c2)
        m (incr/imap {:a c1 :b t2})]
    (incr/set-active! m)
    (incr/stabilize!)
    (is (= {:a 1 :b 2} @m))
    (is (= 1 @t2-count))

    (swap! c1 inc)
    (incr/stabilize!)
    (is (= {:a 2 :b 2} @m))
    (is (= 1 @t2-count))))

(deftype TestException [])
(set! (.-prototype TestException) (js/Error.))

(deftest exceptions
  (let [ex (TestException.)
        t1 (incr/thunk
             (throw ex))
        t2 (incr/thunk (inc @t1))
        m (incr/imap {:a t1})
        ]
    (incr/set-active! t2)
    (incr/set-active! m)
    (incr/stabilize!)
    (is (thrown? TestException @t2))
    (is (= @m {:a ex}))
    ))

(deftest map-vals
  (let [c (incr/cell {:a 10
                      :b 20})
        increased (atom #{})
        t (incr/map-vals c (fn [x]
                             (swap! increased conj x)
                             (inc x)))]
    (incr/set-active! t)
    (incr/stabilize!)
    (is (= @t {:a 11 :b 21}))
    (is (= @increased #{10 20}))
    (swap! c update :a inc)
    (incr/stabilize!)
    (is (= @t {:a 12 :b 21}))
    (is (= @increased #{10 11 20}))

    (is (= (incr/map-vals {:a 1} inc) {:a 2}))
    ))
)