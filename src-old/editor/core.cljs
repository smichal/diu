(ns editor.core
  (:require [ui-des.store :as s]
            [ui-des.dom :as d]
            [ui-des.computed :as c]))


(def t
  (s/ent {:txt "hello1 "}))

(defn span [txt]
  {:tag "span"
   :children [{:tag :text
               :text txt}]})

(def span-cmp
  (s/ent
   {:render (fn [props]
              {:tag "span"
               :children [{:tag :text
                           :text (c/value (:txt props))}]})}))

(def clist
  (s/ent
   {:render (fn [props]
              {:tag "div"
               :children (mapv
                          #(assoc-in (:cmp-template props) [:props (:item-field-name props)] %)
                          (c/value (:items props)))})
    :props {:cmp-template {:type :part
                           :name "Part template for items"}
            :item-field-name {:type :keyword :name "Field name"} ;; prop?
            :items {:type :data :name "Items"}  ;; collection,
}
    :meta {:name "List"}}))

(def item
  (s/ent
   {:render (fn [props]
              {:tag "div"
               :children [{:part span-cmp :props props}
                          {:part span-cmp :props t}
                          {:part span-cmp :props {:txt (:txt t)}}]})
    :props {:txt {:type :string
                  :name "Text"
                  :default "<text>"
                  :required true}}}))

(def item2
  (s/ent
   {:render (fn [props]
              {:tag "div"
               :children [{:part span-cmp :props props}
                          {:part span-cmp :props {:txt (:txt t)}}]})
    :props {:txt {:type :string
                  :name "Text"
                  :default "<text>"
                  :required true}}}))

(def flex
  (s/ent
   {:render
    (fn [props]
      {:tag "div"
       :attrs {:style "display: flex"}
       :children (:children props)})
    :props {:children {:type :parts}}}))

(def tmp-app
  (s/ent
   {:part clist
    :props {:cmp-template {:part item
                           :props {}}
            :item-field-name :txt
            :items ["1 " "2 " "3 "]}}))

(defn edn-input! [val ent path]
  (try
    (let [val (cljs.reader/read-string val)]
      (s/transact! ent
                   #(assoc-in % path val)))
    (catch js/Exception e)))

(def c-input
  (s/ent
   {:render (fn [props]
              {:tag "div"
               :children [{:tag :text
                           :text (:label props)}
                          {:tag "input"
                           :attrs {:value (c/computed #(pr-str (get-in (:entity props) (:field-path props))))
                                   :oninput #(edn-input! (-> % .-target .-value)
                                                         (:entity props) (:field-path props))}}]})}))

(def part-prop-editor
  (s/ent
   {:render (fn [props]
              (let [cmp (:cmp props)
                    part (:part cmp)
                    field (:p props)]
                {:part c-input
                 :props {:label (get-in part [:props field :name])
                         :entity cmp
                         :field-path [:props field]}}))}))

(def part-editor
  (s/ent
   {:render
    (fn [props]
      (js/console.log "XXX" @(get-in props [:part :part :props]))
      {:tag "div"
       :children [{:tag "h3"
                   :children [{:tag :text
                               :text (get-in props [:part :part :meta :name])}]}
                  {:part clist
                   :props {:cmp-template {:part part-prop-editor
                                          :props {:cmp (:part props)}}
                           :item-field-name :p
                           :items (c/computed
                                   (fn []
                                     (map first @(get-in props [:part :part :props]))))}}]})}))

(defonce _root
  (let [dom-root
        (d/render
         {:part flex
          :props {:children [tmp-app
                             {:part part-editor
                              :props {:part tmp-app}}]}})
        #_(d/render [c1 {:n v}])]

    (js/console.log dom-root)
    (-> (js/document.getElementById "app") (.appendChild (d/mount dom-root)))))