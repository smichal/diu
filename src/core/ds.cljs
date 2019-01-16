(ns core.ds
  (:require
    [datascript.core :as d]
    [core.events :as e]
    [mount.core :as mount :refer [defstate]]
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [core.computed :as c]
    ))

(declare new-db)

(defstate state
          :start {:db (new-db)
                  :subsciptions (atom {:ea {}
                                       :a {}})
                  })

(def schema {:widget/name {:db/unique :db.unique/identity}
             :widget/params {}
             :widget/steps {:db/cardinality :db.cardinality/many
                            :db/valueType :db.type/ref
                            ;:db/isComponent true
                            }
             :widget/base-params {}
             :widget/base {:db/valueType :db.type/ref}

             :widget-step/fn {}
             :widget-step/params {}

             :winst/rid {:db/unique :db.unique/identity}
             :wrapper/for-edit {:db/valueType :db.type/ref}


             :step-editor/step-fn {:db/unique :db.unique/identity}
             :step-editor/widget {:db/valueType :db.type/ref}

             })

;; xxx
(extend-type js/Function
  IComparable
  (-compare [x y]
    (compare (str x) (str y))))

(defn conn [] (:db @state))
(defn db [] @(:db @state))

(defn subsciptions [] (:subsciptions @state))

(defn tx-effect-handler [tx]
  ;(js/console.log "new TX" tx)
  (d/transact! (conn) tx))

(defn tx-report-listener [tx-report]
  (let [datoms (.-tx-data tx-report)]
    (doseq [[e a v _ added?] datoms]
      (let [v (if (= :db.type/ref (get-in schema [a :db/valueType]))
                (d/entity (db) v)                           ;; todo: observable enity
                v)]

        (when-let [obs (get-in @(subsciptions) [:ea [e a]])]
          (if (= :db.cardinality/many (get-in schema [a :db/cardinality]))
            (c/observable-set! obs (if added?
                                     (conj @obs v)
                                     (disj @obs v)))
            (c/observable-set! obs (if added? v nil))))

        (when-let [obs (get-in @(subsciptions) [:a a])]
          (c/observable-set! obs (if added?
                                   (conj @obs v)
                                   (disj @obs v)))))

      )))

(defn sub-entity-attr [eid attr]
  ;(js/console.log "sub-entity-attr" eid attr)
  ;; check for duplicates / cache
  (if-let [eid (or (:db/id eid) (d/entid (db) eid))]
    (if-let [obs (get-in @(subsciptions) [:ea [eid attr]])]
      obs
      ;; duplicate if subsctiption is created but not active (nobody listens)
      (c/observable
        (get (d/entity (db) eid) attr)
        (fn [obs]
          (swap! (subsciptions) assoc-in [:ea [eid attr]] obs))
        (fn [_]
          (swap! (subsciptions) update :ea dissoc [eid attr]))
        #(get (d/entity (db) eid) attr)
        ))
    ;(throw "can not observe not existing entity")
    nil
    ))

(defn sub-attr [attr]
  (if-let [obs (get-in @(subsciptions) [:a attr])]
    obs
    ;; duplicate if subsctiption is created but not active (nobody listens)
    (c/observable
      (->> (d/q [:find '?v
             :where ['?e attr '?v]]
                (db))
           (map first)
           (into #{}))
      (fn [obs]
        (swap! (subsciptions) assoc-in [:a attr] obs))
      (fn [_]
        (swap! (subsciptions) update :a dissoc attr))
      #(->> (d/q [:find '?v
                  :where ['?e attr '?v]]
                 (db))
            (map first)
            (into #{}))
      )))

(defn new-db []
  ;(e/register-effect-handler! :tx tx-effect-handler)
  (let [conn (d/create-conn schema)]
    (d/listen! conn tx-report-listener)
    conn))



(deftest test1
  (mount/stop #'state)

  (d/transact! (conn)
               [{:db/id -1
                 :name  "Maksim"
                 :age   45
                 :aka   ["Max Otto von Stierlitz", "Jack Ryan"] }])
  (let [sub (sub-entity-attr 1 :age)
        reactions (atom [])
        r (c/reaction (fn [] (swap! reactions conj @sub)))]
    (is (= @sub 45))
    (tx-effect-handler [{:db/id 1 :age 46}])
    (is (= @reactions [45 46]))
    (is (= @sub 46))
    (c/stop-reaction r)
    (tx-effect-handler [{:db/id 1 :age 47}])
    (is (= @sub 47))
    (is (= @reactions [45 46]))
    ))