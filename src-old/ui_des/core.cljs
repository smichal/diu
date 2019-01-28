(ns ui-des.core
  (:require ui-des.computed
            ui-des.dom
            ))

(enable-console-print!)


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)


(def store
  (atom
    {:val1 1

     :event-handlers
     {:button-clicked nil
      }

     :root-component
     {:name :c1
      :mixins
      [[:container
        {:children
         [{:name :t1
           :mixins [[:text {:text "Hello world "}]]}
          {:name :t2
           :mixins [[:text {:text [::ref :val1]}]]}
          {:name :b1
           :mixins [[:button {:text "click"
                              :on-click [:button-clicked]}]]}
          ]}]]}
     }))




(def schema
  {::store
   {:fields {:root-component ::component}}
   ::component {:name :string
                :prefab ::component
                :mixins [::mixin-inst]}
   ::mixin-inst {:props nil}
   ::mixin-def {:props ::popps-def
                :render ::fn}}
  )


(defn read [p]
  (get-in @store p))

(defn write [p v]
  (swap! store assoc-in p v))

(defn resolve-prop [v]
  (if (and (vector? v)
           (= (first v) ::ref)
           )
    (read (drop 1 v))
    v))

(defn resolve-props [props]
  (->> props
       (map (fn [[k v]]
              [k (resolve-prop v)]))
       (into {})))

(defn new-event [event-id payload]
  {:event-id event-id
   :payload payload
   :component nil
   :timestamp (.getTime (js/Date.))
   })

(defn dispatch [[event-id data :as e]]
  (println e)

  )


(declare compo)

(def mixins-def
  {:root {:props {:child {}}
          :render (fn [props]
                    (compo (:child props)))}
   :text {:props {:text {}}
          :render (fn [props]
                    [:span {} (:text props)])}


   :button {:props {:text {}
                    :on-click {}}
            :render (fn [props]
                      [:button {:on-click #(dispatch (:on-click props))}
                       (:text props)]
                      )}

   :local-state {:props {}
                 :state {}  ;??
                 ; schema, pointers to local state?
                 }

   :container
   {:props {:children {}}
    :render (fn [props]
              [:div
               (for [c (:children props)]
                 [:div (compo c)])])}

   :contanier2
   {:props {:child1 {}
            :child2 {}}
    :render (fn [props]
              [:div
               (compo (:child1 props))
               (compo (:child2 props))])}})

(def last-componet-id (atom 0))
(defn new-comonent-id []
  (swap! last-componet-id inc))

(def components (atom {}))

#_(rum/defc compo <
  {:init (fn [state props]
           (let [id (new-comonent-id)]

             (swap! components
                    assoc
                    id
                    {:parent nil}
                    )
             (assoc state ::id id)))}
  ;; assign new runtime id
  ;; store parents/static path
  ;; nie tworzyc react-komponentu gdy nie trzeba

  [{:keys [name mixins]}]
  (let [render-fns (->> mixins
                       (keep
                         (fn [[name props]]
                           (when-let [render-fn (get-in mixins-def [name :render])]
                             #(render-fn (resolve-props props))))))]
    (case (count render-fns)
          0 nil
          1 ((first render-fns))
          [:div
           (map #((%)) render-fns)])))


#_(rum/defc root-comp <
  rum/reactive
  []
  (rum/react store)
  [:div
   (compo
     {:name :root
      :mixins [[:root {:child [::ref :root-component]}]]})])


#_(rum/mount (root-comp) js/document.body)


[{:name :vcont1
  :tags []
  :mixins [] ; <- state?
  :children{:slot1 {#_component}
            :slot2 [{#_components}]}
  }

 {:prefab :vvv
  :name :asd}

 ]




; prefabs
[]

; mixins
; - mock data




[{:name :vcont1
  :mixins [{:mixin :vcont
            ; 1, 10%
            }]

  :children
  [{:name :todos-list
    :mixins [:render-list
             :fetch-todos #_:mock-data
             ]
    :children
    [{:list-of {#_component}
      :for :todos}]
    }]}]

;; animation timeline
;;

;; input, -> triggers event
;;


;; store
; component-id -> props



(defn normalize [data])

