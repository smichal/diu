(ns parts.styles
  (:use [runtime.widgets :only [defpart]])
  (:require [incr.core :as incr]))

(defpart
  :set-styles
  :part/augment-result
  (fn [ctx params result]
    (update result
            :dom/styles
            merge
            params)))
