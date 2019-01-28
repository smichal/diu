(ns ui-des.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            ui-des.computed
            ui-des.store
            ui-des.store-data
            ui-des.dom))

(doo-tests
 'ui-des.computed
 'ui-des.store
 'ui-des.store-data
 'ui-des.dom)
