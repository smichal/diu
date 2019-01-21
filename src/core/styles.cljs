(ns core.styles
  (:require [core.incr :as incr]
            ["fela" :refer (createRenderer)]
            ["fela-dom" :refer (render)]
            ["fela-plugin-unit" :default unit]))

(def renderer (createRenderer #js {;:devMode true
                                   :plugins #js [(unit "px")]
                                   }))

(def app-cfg
  {:parts
   {:set-styles {:intercept {:after (fn [ctx params]
                                      ;(js/console.log "CSS" (clj->js params))
                                      ;(js/console.log "CSS2" (.renderRule renderer #(clj->js params)))
                                      (assoc-in ctx [:dom :styles] params
                                                ;(.renderRule renderer #(clj->js params))
                                                #_(incr/memo
                                                    (fn [ctx params]

                                                      )))
                                      #_(assoc-in ctx [:dom :style] params))}
                 :meta {:name "Styles"
                        :doc ""
                        :props {:color {:label "Color" :type :string}
                                :font-size {:label "Font size" :type :string}
                                }
                        }
                 }}})

(def styles-per-elem (js/WeakMap.))

(defn render-rule [elem params]
  (let [old-rules (.get styles-per-elem elem)
        new-rules (js/Object.assign #js {} old-rules (clj->js params))]
    (.set styles-per-elem elem new-rules)
    (.renderRule renderer (fn [_] new-rules))))

(defn init-dom-renderer! []
  (render renderer))