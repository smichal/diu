(ns dom.dom
  (:require [clojure.core.match :refer [match]]
            [com.rpl.specter :as specter]
            [clojure.core.rrb-vector :as fv]))

(def diff-reducers (atom {}))

(deftype Node [ref children])

(def dom-refs
  (atom (Node. js/document.body nil)))

(defn get-node
  ([path node]
   ;(js/console.log "get-node" node path)
   (loop [[a & rest] path
          node node]
     (if a
       (recur rest (get (.-children node) a))
       (some-> node .-ref))))
  ([path]
   (get-node path @dom-refs)))

(defn fv-insert [v i e]
  (if v
    (fv/catvec (conj (fv/subvec v 0 i) e) (fv/subvec v i))
    (fv/vector e)))

(defn fv-dissoc [v i]
  (fv/catvec (fv/subvec v 0 i) (fv/subvec v (inc i))))

(defn inset-ref [node [a & rest] ref]
  (if (seq rest)
    (Node. (.-ref node)
           (update
             (or (.-children node)
                 (if (number? a) (fv/vector) {}))
             a
             #(inset-ref % rest ref)))
    (Node. (.-ref node)
           (if (number? a)
             (fv-insert (.-children node) a (Node. ref nil))
             (assoc (.-children node) a (Node. ref nil))))))


(defn remove-ref [node [a & rest]]
  (if (seq rest)
    (Node. (.-ref node)
           (update
             (.-children node)
             a
             #(remove-ref % rest)))
    (Node. (.-ref node)
           (if (map? (.-children node))
             (dissoc (.-children node) a)
             (fv-dissoc (.-children node) a)))))

(defn remove-refs [node [a & rest]]
  (if a
    (Node. (.-ref node)
           (update
             (.-children node)
             a
             #(remove-refs % rest)))
    (Node. (.-ref node) nil)))

(defn add-node! [path ref]
  ;(js/console.log "add-node!" path)
  (swap! dom-refs inset-ref path ref))

(defn add-diff-reducer [key f]
  (swap! diff-reducers assoc key f))

(defn apply-change [ctx [r & path] op data]
  (let [reducer (get @diff-reducers r)]
    (when-not reducer (throw (str "No dom reducer for: " r)))
    (reducer ctx (vec path) op data)))

(defn add-child [{:keys [node-path] :as ctx} elem-id data]
  (when data
   (let [elem (js/document.createElement (name (:dom/tag data)))
         before (when (number? elem-id)
                  (get-node (conj node-path elem-id)))]
     (add-node! (conj node-path elem-id) elem)
     (.insertBefore (get-node node-path) elem before)
     (let [ctx (update ctx :node-path conj elem-id)]
       (doseq [[k v] (->> data
                          (sort-by (fn [[k v]] (get {:dom/styles -1 :docker-layout/root 2 :docker-layout/frames 3} k 0)))
                          )]
         (apply-change ctx [k] :+ v)))
     )))

(defn add-children [ctx data]
  (doseq [[id desc] (map-indexed (fn [i x] [i x]) data)]
    (add-child ctx id desc)))

(defn remove-child [{:keys [node-path]} elem-id]
  ;(js/console.log "remove-child" node-path elem-id)
  (when-let [elem (get-node (conj node-path elem-id))]
    (.remove elem)
    (swap! dom-refs remove-ref (conj node-path elem-id))))

(defn remove-children [{:keys [node-path]}]
  (swap! dom-refs remove-refs node-path)
  (let [elem (get-node node-path)]
    (while (.-firstChild elem)
      (.remove (.-firstChild elem)))))

(add-diff-reducer
  :dom/children
  (fn [ctx path op data]
    ;(js/console.log ":dom/children" path op data)
    (match [path op data]
      [[:root] :r _] (add-child ctx :root data)
      [[elem-id] :r nil] (remove-child ctx elem-id)
      [[elem-id] :+ _] (add-child ctx elem-id data)
      [[elem-id] :r _] (add-child ctx elem-id data)
      [[elem-id] :- _] (remove-child ctx elem-id)
      [[elem-id & r] _ _] (apply-change (update ctx :node-path conj elem-id) r op data)
      [[] :+ _] (add-children ctx data)
      [[] :- _] (remove-children ctx)
      [[] :r _] (do (remove-children ctx)
                  (add-children ctx data)))))

(add-diff-reducer
  :dom/tag
  (fn [{:keys [node-path]} path op data]
    ;; todo?
    #_(match [path op]
           [[] :r] (let [old-elem (get-node node-path)
                         new-elem (js/document.createElement (name data))]

                     ; change ref in @dom-refs
                     (loop [[a & rest] node-path
                            node @dom-refs]
                       (if a
                         (recur rest (get (.-children node) a))
                         (set! (.-ref node) new-elem)))

                     (.append new-elem (.-children old-elem))

                     (.replaceChild (.-parentElement old-elem)
                                    new-elem
                                    old-elem)

                     ))))

(add-diff-reducer
  :dom/text
  (fn [{:keys [node-path]} path op data]
    (set! (.-innerText (get-node node-path)) data)))

(defn set-attr [elem attr val]
  (let [attr (if (keyword? attr) (name attr) attr)
        val (if (keyword? val) (name val) val)]
    (if val
      (.setAttribute elem attr val)
      (.removeAttribute elem attr)
      )))

(add-diff-reducer
  :dom/attrs
  (fn [{:keys [node-path]} path op data]
    (let [elem (get-node node-path)]
      ;(js/console.log "ATTRs" elem path op data)
      (match [path op]
             [[] :+] (doseq [[k v] data]
                       (set-attr elem k v))
             [[] :-] nil  ;fixme
             [[k] _] (set-attr elem k data)
             ))))

(add-diff-reducer
  :dom/call-id
  (fn [{:keys [node-path]} path op data]
    (set-attr (get-node node-path) "data-id" data)))