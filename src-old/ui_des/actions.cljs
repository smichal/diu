(ns ui-des.actions
  (:require [cljs.core.async :as async]
            [ui-des.effects :as fx]
            ))

(def actions-register (atom {}))


;; todo: interceptors a'la re-frame


(def actions-chan (async/chan 1024))

(defn def-action [name & {:keys [f] :as params}]
  (swap! actions-register
         assoc name params))

;; more context
(defn dispatch [name params]
  (async/put! actions-chan
              [name params]))

(defn- perform [[name params]]
  (if-let [a (@actions-register name)]
    (->> ((:f a) params)
        (map fx/perform)
         dorun)
    (throw (str "no action " name))))

(defn actions-loop []
  (async/go-loop [a (async/<! actions-chan)]
    (perform a)
    (recur (async/<! actions-chan))))
