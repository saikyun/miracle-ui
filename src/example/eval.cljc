(ns example.eval)

(defonce id (atom 0))
(defn id!
  []
  (swap! id inc))

(defn eval-str!
  [connection code]
  (.write connection (str "{:res (pr-str " code "), :id " (id!) "}\n")))

(defmacro eval!
  [connection code]
  `(.write ~connection ~(str "{:res (pr-str " code "), :id " (id!) "}\n")))

(defmacro raw-eval!
  [connection code]
  `(.write ~connection (str ~code "\n")))
