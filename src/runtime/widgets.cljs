(ns runtime.widgets
  (:require [clojure.spec.alpha :as s]
            clojure.test.check.generators
            clojure.test.check.properties
            clojure.test.check
            [incr.core :as incr]
            [com.rpl.specter :as specter]))

(s/def ::part
  (s/keys
    :req [(or :part/render :part/augment-ctx :part/augment-result)]

    ))

(s/def ::result (s/keys))
(s/def ::ctx (s/keys))

(s/def :part/name string?)
(s/def :part/desc string?)
#_(s/def :part/render (s/fspec :args (s/cat :ctx ::ctx
                                          :params (s/map-of keyword? any?))
                             :ret ::result))
#_(s/def :part/augment-ctx (s/fspec :args (s/cat :ctx ::ctx
                                               :params (s/map-of keyword? any?))
                                  :ret ::ctx))
#_(s/def :part/augment-result (s/fspec :args (s/cat :ctx ::ctx
                                                  :params (s/map-of keyword? any?)
                                                  :result ::result)
                                     :ret ::result))

(s/def :part/params (s/map-of keyword? :part/param))
(s/def :part/param (s/keys))

(defprotocol IWithCtx
  (-with-ctx [this ctx]))

(deftype WithCtx [f]
  IWithCtx
  (-with-ctx [this ctx]
    (f ctx)))

(defn with-ctx [f] (WithCtx. f))

(defn deep-ctx-apply [ctx x]
  (specter/transform
    (specter/walker #(implements? IWithCtx %))
    #(-with-ctx % ctx)
    x))

(def parts (atom {}))

(defn defpart [id & {:as m}]
  (when-not (s/valid? ::part m)
    (js/console.warn m (s/explain-str ::part m))
    (throw (js/Error. "Part invalid")))
  (swap! parts assoc id m))



#_(s/def ::widget
  (s/keys
    :opt [:widget/meta
          :order]
    ))

(defn map-in-order [m]
  (let [order (:order m)
        not-ordered (clojure.set/difference (set (keys m)) (set order) #{:order})]
    (concat
      (map (fn [k] [k (m k)]) order)
      (map (fn [k] [k (m k)]) not-ordered))))

;; parts from ctx?
(defn compose-parts [ctx w]
  ;(js/console.log "compose-parts" w)
  (let [parts-defs (::parts ctx)
        part-calls (map-in-order w)
        parts-num (count part-calls)
        part-calls (map-indexed (fn [idx [id params]]
                          (let [part (get parts-defs id)]
                            (if-not part
                              (if (not= idx (dec parts-num))
                                (throw (js/Error. (str "Part not found: " id)))

                                ;; widget can by called instead of part for last part
                                [(get parts-defs :widget) {:widget id
                                                           :params params}])
                              [part params])))
                        part-calls)]
    (let [ctx (reduce
                (fn [ctx [part params]]
                  (if-let [f (:part/augment-ctx part)]
                    (f ctx
                       (deep-ctx-apply ctx params))
                    ctx))
                ctx
                part-calls)
          [last-part last-part-params] (last part-calls)
          result ((:part/render last-part)
                  ctx
                  (deep-ctx-apply ctx last-part-params)) ;; auto resolve widget?
          result (reduce
                   (fn [result [part params]]
                     (if-let [f (:part/augment-result part)]
                       (f ctx
                          (deep-ctx-apply ctx params)
                          result)
                       result))
                   result
                   (reverse part-calls))]
      result)))

(defn resolve-widget [ctx widget-or-id]
  (if (keyword? widget-or-id)
    (get-in ctx [::widgets widget-or-id])
    widget-or-id))

(defn call [ctx w param-path]
  ;(js/console.log "CALL" w)
  (incr/deep-deref
    @(incr/incr compose-parts
                (update ctx ::param-path #(concat % param-path))
                (incr/value w))))

;; :part/render (ctx: const), (params: (possible?) thunk)
;; children (id -> Thunk(x)) ?


(deftype CtxGetter [path]
 IWithCtx
  (-with-ctx [this ctx]
    (get-in ctx (deep-ctx-apply ctx path)))
  ;Object
  ;(toString [this] (pr-str path))
  IEquiv
  (-equiv [this other]
    (= path (.-path other)))
  ;IPrintWithWriter
  ;(-pr-writer [_ writer _]
  ;  (write-all writer "#ctx" (pr-str (vec path))))
  )

(defn gctx [& args]
  (CtxGetter. args))
