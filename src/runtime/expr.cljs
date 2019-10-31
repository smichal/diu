(ns runtime.expr
  (:require                                                 ;[runtime.widgets :as w]
    [incr.core :as incr]
    [cljs.spec.alpha :as s]
    [com.rpl.specter :as specter]
    ;[clojure.core.match :refer [match]]
    ))


#_(deftype CtxGetter [path]
  w/IWithCtx
  (-with-ctx [this ctx]
    (get-in ctx path)))

#_(defn ctx-get [& path]
  (CtxGetter. path))


(defn parts-for-search [parts]
  (->> parts
       (map (fn [[k v]]
              {:name (or (:part/name v) (str k))
               :desc (or (:part/desc v) "")}
              ))))

(def env
  {'= =
   'str str
   'pr-str pr-str
   'get-in get-in
   'get get
   'map map
   'map? map?
   'mapcat mapcat

   'butlast butlast

   'hash hash

   'parts-for-search parts-for-search
   })

(defn eval-expr
  ([ctx expr]
   (let [res
         (cond

           (and (env expr) (symbol? expr)) (env expr)

           (list? expr)
           (case (first expr)
             if (let [[_ c t e] expr]
                   (if (eval-expr ctx c)
                     (eval-expr ctx t)
                     (eval-expr ctx e)))
             or (let [[_ & args] expr]
                   (first (keep #(eval-expr ctx %) args)))
             and (let [[_ & args] expr]
                    (every? #(eval-expr ctx %) args))

             params (let [[_ & path] expr]
                      (reduce
                        (fn [o k] (incr/value (get o k)))
                        (:params ctx)
                        (map (partial eval-expr ctx) path)))

             scope (let [[_ & path] expr]
                      (reduce
                        (fn [o k] (incr/value (get o k)))
                        (:scope ctx)
                        (map (partial eval-expr ctx) path)))

             ctx (let [[_ & path] expr]
                    (when (or (< 1 (count path))
                              (not (#{:params :scope} (first path)))) ;; todo remove
                      (reduce
                        (fn [o k] (incr/value (get o k)))
                        ctx
                        (map (partial eval-expr ctx) path))))

             ;; fn call
             (let [[f & args] expr]
               (apply (eval-expr ctx f)
                      (->> args
                           (map (partial eval-expr ctx))))))

           (vector? expr)
           (mapv (partial eval-expr ctx) expr)

           (map? expr)
           (->> expr
                (map (fn [[k v]]
                       [(eval-expr ctx k) (eval-expr ctx v)]))
                (into {}))

           ;(implements? w/IWithCtx expr)
           ;(w/-with-ctx expr ctx)

           :else
           expr)
         res (incr/value res)
         ;;res (if (implements? IDeref res) nil res)
         ]
     ;(js/console.log "=>" expr " => " res)
     res))
  ([ctx expr spec]
   (let [res (eval-expr ctx expr)]
     (when (or (nil? spec)
               (s/valid? spec res))
       res))))

#_(deftype Expr [expr spec]
  w/IWithCtx
  (-with-ctx [this ctx]
    ;(js/console.log "EVAL" expr ctx)
    (incr/incr eval-expr ctx expr spec))
  IEquiv
  (-equiv [o other]
    (and (= (type o) (type other))
         (= expr (.-expr other))))
  IHash
  (-hash [this]
    (-hash expr)))


(defn expr [e]
  e
  #_(specter/transform
    (specter/codewalker list?)
    #(Expr. % nil)
    e))

#_(defn expr
  ([e] (Expr. e nil))
  ([e spec] (Expr. e spec)))

