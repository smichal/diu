(ns core.widgets
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [core.ds :as ds]
            [core.computed :as c]
            [datascript.core :as d]))

(def log js/console.log)

#_(defn widget-recipe-to-desc [recipe parent-ctx]
  (if (nil? recipe)
    {:context parent-ctx}
    ((:fn recipe)
      (widget-recipe-to-desc (:next recipe) parent-ctx)
      (:params recipe))))


#_(deftest test-widget-recipe-to-desc
  (is (= (widget-recipe-to-desc nil {}) {}))
  (is (= (widget-recipe-to-desc {:fn #(assoc %1 :a %2) :params 1} {}) {:a 1})))

#_(defn widget-recipe [eid]
  (c/computed (fn []
                {:steps @(ds/sub-entity-attr eid :widget/steps)
                 :params @(ds/sub-entity-attr eid :widget-transformation/params)
                 :next (some-> @(ds/sub-entity-attr eid :widget-transformation/next)
                               :db/id widget-recipe deref)})))

(defn widget-desc [eid next-params parent-ctx]
  (c/computed
    (fn []                                                  ;; watch full entity instead
      (if eid
        (let [steps @(ds/sub-entity-attr eid :widget/steps)
              base-params @(ds/sub-entity-attr eid :widget/base-params)
              base @(ds/sub-entity-attr eid :widget/base)
              base-widget-desc (if base
                                 @(widget-desc base base-params parent-ctx)
                                 {:context parent-ctx})]
          (log "widget-desc" steps)
          (reduce (fn [widget step]
                    (let [_ (log "widget-desc step" step )
                          f (:widget-step/fn step)
                          params @(ds/sub-entity-attr (:db/id step) :widget-step/params)
                          ;_ (js/console.log "PARAMS" params)
                          ;params (->> params
                          ;            ;; fixme, unify with ctx?
                          ;            (map (fn [[k v]] [k (get next-params v v)]))
                          ;            (into {}))
                          ]
                      ;(js/console.log "params" step params)
                      (f widget params)))
                  (update-in base-widget-desc
                             [:context :params]
                             merge next-params)
                  #_(reverse steps)
                  (reverse (sort-by :widget-step/priority steps))
                  ))
        {:render (fn [] (js/document.createComment "empty"))} ;; xxx
        ))))

(deftest test1
  (ds/tx-effect-handler [{:widget/name :test
                          :widget/steps [{:widget-step/fn #(assoc %1 :v (:a %2))
                                          :widget-step/params {:a 1}}]}])
  (is (= {:context {:params nil} :v 1} (widget-desc [:widget/name :test] nil {})))
  ;(ds/tx-effect-handler [{:widget/name :test :widget-transformation/params 2}])
  ;(is (= {:a 3} (widget-recipe-to-desc @(widget-recipe [:widget/name :test]) {})))
  )



(defn compose-for-db [steps name]
  (cond-> {:widget/steps (vec (map-indexed (fn [i [f p]] {:widget-step/fn f :widget-step/params p :widget-step/priority i}) steps))}
          name (assoc :widget/name name)))

#_(deftest compose-for-db-test
  (js/console.log (compose-for-db [base-widget-t]))
  (ds/tx-effect-handler [base-widget-t])
  )