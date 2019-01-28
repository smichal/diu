(ns ui-des.draft
  (:require [com.rpl.specter :as s]))






"""
template: html, parts
render: desc


parts/modifiers -> comp-desc
comp-desc -> render

- template
- 



editor
- v-flex


flex
style: {}
children: []
template: <div style=${style}>${children}</div>


set-flex-params: (saved/ui vals, editor-api) -> desc->desc-with-style, UI

part: (desc-mapper, ui, default-state)


comp flex:
[[set-flex-params {}]
 [set-children :children]
 [props {:children :children
         :title :String}]]

comp button:
[props {:on-click ..., :text ...}]
template: <button on-click=${on-click}>${text}</button>
// render: default impl

"""

(defn add-lit-renderer [{:keys [template] :as props}]
  ;render template(props)
  )

(defn )








