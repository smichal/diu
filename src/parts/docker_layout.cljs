(ns parts.docker-layout
  (:use [runtime.widgets :only [defpart resolve-widget call]])
  (:require [incr.core :as incr]))

(defpart
  :docker-layout
  :part/render
  (fn [ctx {:keys [layout frames]}]
    {:dom/tag :div
     :docker-layout/root {:layout layout}
     :docker-layout/frames (->> frames
                                (map (fn [[k w]]
                                       [k [(incr/incr call ctx w)]]))
                                (into {}))}
    ))