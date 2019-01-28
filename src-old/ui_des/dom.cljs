(ns ui-des.dom
  (:require [smidjen.core :refer-macros [facts fact]]
            [ui-des.computed :as c]
            [petit-dom :as p]
            [ui-des.store :as store]
            [com.rpl.specter :as sp]))

(def dom-signals (atom {}))

(defn part->render-fn [part]
  (com/value (:render part)))

#_(defn part-usage->props [part-usage]
    (:props part-usage))


(defn dom-signal-derefer [form]
  (if (and (map? form) (:part form))
    form
    (do
      ;(js/console.log "SD" form)
      (clojure.walk/walk (partial dom-signal-derefer)
                         com/value
                         form))))

(declare render)

;; comp-def, -> dom desc
(defn part-renderer [part props]
  (com/computed
   (fn []
     (let [render-fn (part->render-fn part)
            ;; todo: rethink
            ;props (part-usage->props part-invocation)
           result (render-fn @props #_(merge props dom-props))
           result (dom-signal-derefer result)]
        ; result -> intermediate form of dom
        ; (render result) -> petit-dom's representation of dom
       result))))

(defn update-dom-reaction [cmp observable-dom]
  (com/reaction
   (fn []
     (when (.-_vnode cmp)
       (let [vnode (render @observable-dom)]
         (p/patch vnode (.-_vnode cmp))
         (set! (.-_vnode cmp) vnode))))))

(defn component-prototype [cmp]
  #js{:mount (fn [props content]
               (this-as this
                 ;; props??
                        (when content
                          (throw "content not supported, pass children in props"))
                        (set! (.-props this) (com/observable props (fn []) (fn [])))

                        (let [dom-desc (part-renderer cmp props)
                              vnode (render @dom-desc)]
                          (set! (.-_vnode this) vnode)
                   ;(set! (.-_sig this) dom-desc-sig)
                          (js/console.log "mount" this #_vnode)
                          (p/mount vnode))))
      :unmount (fn [dom-node]
                 (this-as this
                          (swap! dom-signals dissoc this #_(.-_sig this))
                          (js/console.log "unmount" this #_(.-_vnode this))
                          (p/unmount (.-_vnode this))))
      :patch (fn [dom-node props old-props _ _]
               (this-as this
                        (com/observalbe-set! (.-props this) props)
                        dom-node
                        #_(when (not= old-props props)
                   ;; todo: check
                            (set! (.-_sig this) (part-renderer cmp props))
                            (swap! dom-signals assoc this (.-_sig this))

                            (let [vnode (render (com/value (.-_sig this)))
                                  old-vnode (.-_vnode this)]
                              (set! (.-_vnode this) vnode)
                              (js/console.log "cmp patch" old-props props this)
                              (p/patch vnode old-vnode)
                              dom-node))))})

(def component-wrapper*
  (memoize
   (fn [cmp]
     (js/console.log "NEW cmp " cmp)
     (let [proto (component-prototype (com/value cmp))
           res (fn [_])]
       (set! (.-prototype res) proto)
       res))))

(defn component-wrapper [cmp]
  (component-wrapper* (com/value cmp)))


(def update-scheduled? (atom false))

(defn update-dom! []
  (js/console.log "update-dom!")
  (doseq [[cmp sig] @dom-signals]
    ;; todo: topo sort?
    (let [old-val @(.-val sig)
          val (com/value sig)]    ;; xxx: only form block
      (when (not= old-val val)
        (let [vnode (render val)]
          (js/console.log "patch" old-val val                          ; (.-_vnode cmp) vnode
)
          (p/patch vnode (.-_vnode cmp))
          (set! (.-_vnode cmp) vnode)))))
  (reset! update-scheduled? false))

(defn schedule-update! []
  (when-not @update-scheduled?
    (reset! update-scheduled? true)
    (js/requestAnimationFrame update-dom!)))

(defn render-dom [{:keys [tag attrs children text]}]
  (case tag
    :text #js{:_text text}
    (p/h tag
         (clj->js attrs)
         (clj->js (map render children)))))

(defn render [node]
  (cond
    (:tag node) (render-dom node)
    (:part node) (p/h (component-wrapper (:part node)) (:props node))))


(defn mount [cmp]
  (p/mount cmp))


(facts
 "dom"

 @(part-renderer {:props {:a 1}
                  :render (fn [p] [:div (:a p)])}
                 {})
 => [:div 1]

 (let [a (atom 1)
       x (sig/atom-source a)]
   @(part-renderer {:props {}
                    :render (fn [p] [:div @x])} {}) => [:div 1]
   (swap! a inc)
   @(part-renderer {:props {}
                    :render (fn [p] [:div @x])} {}) => [:div 2]))

#_(let [cmp (store/add-entity {:render (fn [_] [:div {} "cmp"])
                               :props {}})
        dom-root (render [cmp {}])]

    (js/document.body.appendChild (p/mount dom-root)))










