(ns parts.fns)

(defonce fns-registry (atom {}))

(defn register-fn
  [symbol
   fn
   & {:keys [:desc :args :returns]}]

  )

(defn subtype? [a b]
  (= a b))

(register-fn
  'count count
  :desc "Returns the number of items in the collection."
  :args [[seq? {}]]
  :returns number?)

(register-fn
  'scope
  :desc ""
  :args (fn [set-args cxt expected-result-type])
  :returns (fn [ctx])
  )
