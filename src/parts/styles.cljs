(ns parts.styles
  (:use [runtime.widgets :only [defpart]])
  (:require [incr.core :as incr]))

(defpart
  :set-styles
  :part/name "Set styles"
  :part/desc "Sets styles of element"
  :part/augment-result
  (fn [ctx params result]
    (update result
            :dom/styles
            merge
            params)))
