(ns ui-des.query)



(defprotocol IQuery
  
  )


;;
;; query
; readable / printable
; deref
; on-change? / deref z bindowane sledzenie uzycia
; ready? error?
; normalized-to store

; graphql, time, json, img?
; cache in store, ttl?
;

; (defprotocol IEntity) / :db/id


; event -> reductor -> events*


; change-x -> transaction -> filter -> apply ->