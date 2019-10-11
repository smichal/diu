(ns dom.styles
  (:require [dom.dom :as dom :refer [add-diff-reducer]]
            [clojure.core.match :refer [match]]
            [com.rpl.specter :as specter]
            ["fela" :refer (createRenderer)]
            ["fela-dom" :refer (render)]
            ["fela-plugin-unit" :default unit]
            ))

(def styles-per-elem (js/WeakMap.))
(defonce style-renderer (createRenderer #js {:devMode true
                                             :plugins #js [(unit "px")]}))

(add-diff-reducer
  :dom/styles
  (fn [{:keys [node-path]} path op data]
    (let [elem (dom/get-node node-path)
          old-rules (.get styles-per-elem elem)
          new-rules (specter/setval path (or data specter/NONE) old-rules)
          ;_ (js/console.log "Rules " path new-rules)
          cls (.renderRule style-renderer (fn [_] (clj->js new-rules)))]
      (.set styles-per-elem elem new-rules)
      (set! (.-className elem) cls))
    ))


(defn insert-styles! []
  (render dom.styles/style-renderer)
  )