(ns example.eval)

(enable-console-print!)

(defonce id (atom 0))
(defn id!
  []
  (swap! id inc))

(defn eval-str!
  [connection code]
  (.write connection (str "{:res (try (pr-str " code ") (catch js/Error e {:error (pr-str e)})), :id " (id!) "}\n")))

(defmacro eval!
  [connection code]
  `(.write ~connection ~(str "{:res (pr-str " code "), :id " (id!) "}\n")))

(defmacro raw-eval!
  [connection code]
  `(.write ~connection (str ~code "\n")))
