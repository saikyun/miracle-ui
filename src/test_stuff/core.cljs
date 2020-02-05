(ns test-stuff.core
  (:require 
   [clojure.repl :as r :refer-macros [doc]]
   [miracle.save :as ms :refer-macros [save watch]]))

(defn ye
  [x]
  (apply * (range 1 (/ x 100))))

(defn ye1
  [x]
  (+ x x))

(comment
  
  (ye 1337)
  
  (do
    (ye 10)
    (ye 15)
    (ye 20)
    
    (ye1 10)
    (ye1 15)
    (ye1 20)
    
    (ye3 10)
    (ye3 15)
    (ye3 30))
  
  (doseq [i (range 100)
          :let [x (rand-int i)]]
    (try
      (ye3 x)
      (catch js/Error e
        (println "nobody cares"))))
  )

(defn ye3
  [x]
  (if (and (< x 50)
           (> x 25))
    (throw (js/Error. "nooo"))
    (+ x x)))

(defn lule
  [i]
  i)

(defn watch-ns-form
  [ns-sym]
  `(->> (keys (ns-interns '~ns-sym))
        (map #(symbol ~(str ns-sym) %))
        (map #(list 'miracle.save/watch %))
        (cons 'do)))

(lule 5)

(comment
  (watch lule)
  )

(defn start
  []
  (println "starting"))

(defn stop
  []
  (println "stopping"))

(defn main
  []
  (println "test-stuff main")
  (start))
