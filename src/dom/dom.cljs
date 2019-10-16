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
       (.-ref node))))
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
  (let [elem (js/document.createElement (name (:dom/tag data)))
        ;path (if (= id :root) path (conj path id))
        ]
    (add-node! (conj node-path elem-id) elem)
    (.append (get-node node-path) elem)
    (let [ctx (update ctx :node-path conj elem-id)]
      (doseq [[k v] (->> data
                         (sort-by (fn [[k v]] (get {:dom/styles -1 :docker-layout/root 2 :docker-layout/frames 3} k 0)))
                         )]
        (apply-change ctx [k] :+ v)))
    ))

(defn add-children [ctx data]
  (doseq [[id desc] (map-indexed (fn [i x] [i x]) data)]
    (add-child ctx id desc)))

(defn remove-child [{:keys [node-path]} elem-id]
  ;(js/console.log "remove-child" node-path elem-id)
  (let [elem (get-node (conj node-path elem-id))]
    (.remove elem))
  (swap! dom-refs remove-ref (conj node-path elem-id)))

(defn remove-children [{:keys [node-path]}]
  (swap! dom-refs remove-refs node-path)
  #_(swap! dom-refs
         #(specter/setval (concat node-path [specter/MAP-KEYS (comp not #{::ref})])
                          specter/NONE
                          %))
  (let [elem (get-node node-path)]
    (while (.-firstChild elem)
      (.remove (.-firstChild elem)))))

(add-diff-reducer
  :dom/children
  (fn [ctx path op data]
    ;(js/console.log ":dom/children" path op data)
    (match [path op]
      [[:root] :r] (add-child ctx :root data)
      [[elem-id] :+] (add-child ctx elem-id data)
      [[elem-id] :-] (remove-child ctx elem-id)
      [[elem-id & r] _] (apply-change (update ctx :node-path conj elem-id) r op data)
      [[] :+] (add-children ctx data)
      [[] :-] (remove-children ctx))))

(add-diff-reducer
  :dom/tag
  (fn [ctx path op data]
    ;; todo?
    ))

(add-diff-reducer
  :dom/text
  (fn [{:keys [node-path]} path op data]
    (set! (.-innerText (get-node node-path)) data)))

(defn set-attr [elem attr val]
  (if val
    (.setAttribute elem attr val)
    (.removeAttribute elem attr)
    ))

(add-diff-reducer
  :dom/attrs
  (fn [{:keys [node-path]} path op data]
    (let [elem (get-node node-path)]
      (match [path op]
             [[] :+] (doseq [[k v] data]
                       (set-attr elem (name k) (and v (name v))))
             [[k] _] (set-attr elem (name k) (and data (name data)))
             ))))

(add-diff-reducer
  :dom/call-id
  (fn [{:keys [node-path]} path op data]
    (set-attr (get-node node-path) "data-id" data)))