(ns core.dom
  (:require
    [cljs.test :refer-macros [deftest is testing run-tests]]
    [core.computed :as c]
    [mount.core :as mount]
    [core.widgets :as w]
    ["jss" :default jss]
    [mount.core :as mount :refer [defstate]]
    ;["jss-preset-default" :default jss-preset]
    ))

(declare render)

(def last-runtime-widget-id (atom 0))
(defn next-runtime-id [] (swap! last-runtime-widget-id inc))

(defstate sheet
          :start (do
                   (. jss setup)
                   (let [sheet (. jss createStyleSheet #js {} #js {:link true})]
                     (. sheet attach)
                     sheet))
          :stop (. jss removeStyleSheet @sheet))

(def event-types ["click" "change" "input"])
(def event-elem-handler
  (->> event-types
       (map (fn [t] [t (js/WeakMap.)]))
       (into {})))

(defn render-children! [elem children]
  ;(js/console.log "render-children!" elem children)
  (if (implements? IDeref children)
    @(c/computed (fn []
                   #_(while (.-firstChild elem)
                     (.removeChild elem (.-firstChild elem)))
                   (set! (.-innerHTML elem) "")
                   (render-children! elem @children)
                   nil))

    ;; constant children count
    (let [children (if (sequential? children) children [children])]
      (doseq [c children]
        (let [node (render c)]
          (if (implements? IDeref node)
            (let [old (volatile! nil)]
              @(c/computed (fn []
                             (let [new @node]
                               (if @old
                                 (.replaceChild elem new @old)
                                 (.appendChild elem new))
                               (vreset! old new))
                             nil)))
            (.appendChild elem node)))))))

(defn styles-class-name [styles]
  (c/computed
    (fn []
      (let [styles (if (implements? IDeref styles)
                     @styles
                     styles)
            name (or (:id styles) (str "c" (hash styles)))
            styles (dissoc styles :id)
            rule (if-let [r (. @sheet getRule name)]
                   r
                   (. @sheet addRule name (clj->js styles)))]
        ;; if property is computed then create watcher and update only one prop


        ;; clean up
        ;(js/console.log rule)
        ;(.-className rule)
        (.substr (.-selectorText rule) 1)
        ))))

(defn render-tag [w]
  (let [elem (js/document.createElement (name (:tag w)))]
    (doseq [[k v] (:attrs w)]
      (if (implements? IDeref v)
        @(c/computed (fn  []
                      (aset elem (name k) @v)
                      nil))
        ;; setAttribute
        (aset elem (name k) v)))

    (when (:styles w)
      (. (.-classList elem) add @(styles-class-name (:styles w))))

    (when (:events w)
      (doseq [[type handler] (:events w)]
        (.set (event-elem-handler type) elem handler)))

    (when-let [cs (:children w)]
      (render-children! elem cs))

    elem))

(defn render-text [v]
  ;; replace content instead of node
  (let [elem (js/document.createTextNode (if (implements? IDeref v) @v v))]
    (when (implements? IDeref v)
      @(c/computed (fn []
                     (set! (.-innerText elem) @v)
                     nil)))
    elem))

(defn widget-desc [w]
  ;(js/console.log "widget-desc" w)
  (if (:render w)
    w
    @(w/widget-desc (:widget w) (:params w) (:ctx w))))

(defn render-widget [w]
  (let [rid (next-runtime-id)]
    (c/computed
      (fn []
        (let [w (widget-desc w)
              render-fn (:render w)
              ctx (:context w)
              ctx (assoc ctx ::rid rid
                             ::parent (::rid ctx))
              dom (render-fn ctx)]
          ;(js/console.log "render-widget" w)
          ;(js/console.log "render-widget dom" dom)
          (render dom))))))

(defn render [w]
  (cond
    (:tag w) (render-tag w)
    (contains? w :text) (render-text (get w :text ""))
    :else (render-widget w)))

(def log js/console.log)

(defonce _handlers
         (doseq [t event-types]
           (let [handlers (get event-elem-handler t)]
             (js/document.body.addEventListener
               t
               (fn [e]
                 ;; todo: bubble?
                 (when-let [handler (.get handlers (.-target e))]
                   (handler e)))))))



(deftest test1
  (mount/stop)
  @sheet

  (let [o1 (c/observable 2 (fn []) (fn []))]
    (c/reaction (fn []
                  (log "AA"
                       (render {:tag :div
                                :attrs {:id "test1" :className "cls cls2" :title o1}
                                :children (c/computed (fn [] (vec (repeat @o1 {:tag :p}))))
                                }))))

    (c/reaction (fn []
                  (log "BB"
                       (set! js/window.aaa
                         (render {:tag :div
                                  :styles {:color :red}
                                  :children ["asd" #_{:widget [:widget/name :base]} "qwe"]})))))

    (c/observable-set! o1 3)
    )

  )
