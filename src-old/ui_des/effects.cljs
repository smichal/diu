(ns ui-des.effects)


(def effects-register (atom {}))

(defn def-effect [name & {:keys [f] :as params}]
  (swap! effects-register
         assoc name params))

(defn perform [[name & params]]
  (apply
    (@effects-register name)
    params))