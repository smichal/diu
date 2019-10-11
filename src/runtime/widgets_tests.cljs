(ns runtime.widgets-tests
  (:require
    [incr.core :as incr :include-macros true]
    [runtime.widgets :as w]
    clojure.test.check.generators
    clojure.test.check.properties
    clojure.test.check
    [cljs.test :as test :refer-macros [deftest is testing run-tests]]
    [editscript.core :as ec]
    ;[editscript.edit :as ee]
    ))


#_(deftest defpart1
  (reset! w/parts {})
  (is (thrown? js/Error (w/defpart :a :q 1)))
  (w/defpart :test1
             :part/render (fn [ctx params] {})
             )
  (is (get-in @w/parts [:test1 :part/render]))
  )

#_(deftest test1
  (js/console.log "test1")
  (let [c (incr/cell {:a 1 :b 2 :c 1})

        _ (js/console.log @w/parts)
        r (w/call {::w/parts @w/parts}
                  {:dom {:tag :div
                         :children
                         {:a {:dom {:tag :p
                                    :text (:a c)}}
                          :b {:dom {:tag :p
                                    :text (:b c)}}}}})

        _ (incr/set-active! r)
        _ (incr/stabilize!)
        r1 @r
        _ (swap! c update :b inc)
        _ (incr/stabilize!)
        r2 @r
        ]

    (js/console.log "RESULT" r1)
    (js/console.log "RESULT2" r2)

    (js/console.log "DIFF" (ec/diff {} r2))
    (js/console.log "DIFF" (ec/diff r1 r2))

    ))
