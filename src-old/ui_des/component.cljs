(ns ui-des.component)

(comment
  {:name "for editor"
   :props {:field {:type :number
                    :default 1}}
   :locals {}
   :parts [{:part uuid
            :props {}}]
   :prefab 1 #_?
   }

  ;;part
  {:name ""
   :props {}
   :render-fn 1
   }

  ;; part instance/invocation
  {:part 1 #_ref
   :props {}}

  ;; part-def
  {:name ""
   :props {}
   :render (fn [params] ;; other context instead of params?
             )
   }

  ;; assembly - part added in editor, contains others parts

  )



(defn cmp->render-fn [cmp])