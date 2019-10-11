(ns incr.core)

#_(defmacro thunk [& body]
  `(thunk-f (fn [] ~@body)))


(defmacro thunk [& body]
  (let [f (gensym "thunk#")]
    `(do
       (when (not ~f) (defn ~f [] ~@body))
       (incr ~f))))


