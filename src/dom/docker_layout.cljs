(ns dom.docker-layout
  (:require [dom.dom :as dom :refer [add-diff-reducer]]
            [clojure.core.match :refer [match]]
            [com.rpl.specter :as specter]
    ;["zepto" :as zepto]
    ;["golden-layout" :as GoldenLayout]
            ))

(def GoldenLayout js/GoldenLayout)

(add-diff-reducer
  :docker-layout/root
  (fn [{:keys [node-path]} path op data]
    (let [elem (dom/get-node node-path)
          layout (GoldenLayout. (clj->js {:settings {:showMaximiseIcon false
                                                     :showCloseIcon false
                                                     :showPopoutIcon false}
                                          :content [(:layout data)]})
                                elem)
          ]
      (.registerComponent layout "frame"
                          (fn [container]
                            (let [ref (.get (.getElement container) 0)
                                  id (-> container .-_config .-id keyword)]
                              (dom/add-node! (conj node-path id) ref))))
      ;(js/setTimeout #(.init layout) 16)
      (.init layout)
      )))

(add-diff-reducer
  :docker-layout/frames
  (fn [ctx path op data]
    (match [path op]
           [[] _] (doseq [[frame-id data] data]
                    (dom/apply-change (update ctx :node-path conj frame-id) [:dom/children] op data))
           [[frame-id & r] _] (dom/apply-change (update ctx :node-path conj frame-id) (concat [:dom/children] r) op data))
    ))