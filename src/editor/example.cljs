(ns editor.example
  (:require [incr.core :as incr]))


(defn toggle-done [e ctx]
  [[:set-local
    :todos
    (vec (replace {(:item e) (update (:item e) :done not)}
                  @(get-in ctx [:scope :todos])))
    ]])

(defn add-new-item [e ctx]
  [[:set-local
    :todos
    (conj
      @(get-in ctx [:scope :todos])
      {:text (:text e) :done false})]
   [:set-local :text ""]
   ])