(ns dom.vaadin-elements
  (:require
    ["@vaadin/vaadin-button"]
    ["@vaadin/vaadin-text-field"]
    ["@vaadin/vaadin-dialog"]
    ["@vaadin/vaadin-lumo-styles"]
    [clojure.core.match :refer [match]]
    [dom.dom :as dom :refer [add-diff-reducer]]))



(defn create-dialog [{:keys [node-path] :as ctx} data]
  (let [elem (dom/get-node node-path)
        content (js/document.createElement "div")]
    (dom/add-node! (conj node-path 0) content)
    (set! (.-renderer elem)
          (fn [root dialog]
            (.append root content)))
    (dom/set-attr elem :opened (:opened data))
    (dom/add-children (update ctx :node-path conj 0) (:content data))
    ))

(add-diff-reducer
  :vaadin/dialog
  (fn [{:keys [node-path] :as ctx} path op data]
    (match [path op]
           [[] :+] (create-dialog ctx data)
           [[:opened] _] (dom/set-attr (dom/get-node node-path) :opened data)
           [[:content & r] _] (dom/apply-change (update ctx :node-path conj 0) (concat [:dom/children] r) op data)
           )))
