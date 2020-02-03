(ns miracle.save
  (:require [clojure.pprint :refer [pp pprint]]
            [clojure.string :as str])
  (:require-macros [miracle.save :refer [get-env store-var-data var-data arglists]]))

#_(defn eval-in-context
    "Evals the form with the local bindings that have been saved using `save` with `id` as parameter."
    [form bindings id]
    `(let [~@(apply concat (for [[k _] bindings]
                             [k `(-> @miracle.save/saves (get ~id) last second (get '~k))]))]
       ~form))

(defonce id (atom 0))

(defn gen-id [] (swap! id inc))

(def ^:dynamic *max-saves* "The maximum number of saves per id." 10)

(defn inspect-map
  [map-to-print & {:keys [desired-level safe-count]
                   :or {desired-level 4 safe-count 10}}]
  (binding [*print-level* desired-level *print-length* safe-count]
    (clojure.pprint/pprint map-to-print)))

(defn gensym? [s]       
  (re-find #"__\d+" s))

(defonce saves (atom {}))

(defn clear-saves! [] (reset! saves {}))

(defn eval-in-context
  "Evals the form with the local bindings that have been saved using `save` with `id` as parameter."
  ([form id]
   (eval-in-context 
    form 
    id 
    (-> @miracle.save/saves (get id) last first)))
  ([form id pos]
   (let [locals (second
                 (first
                  (filter #(= (first %) pos)
                          (-> @miracle.save/saves (get id)))))
         ks (keys locals)]
     `(let [~'all (second (first (filter #(= (first %) ~pos) (-> @miracle.save/saves (get ~id)))))
            ~@(apply concat (for [k ks] [k `(get ~'all '~k)]))]
        ~form))))

(defn get-last
  [id]
  (let [res (-> @saves (get id) last)]
    [(first res) (with-out-str (inspect-map (second res)))]))

(defn get-last-nof
  [id nof]
  (for [res (reverse (take nof (-> @saves (get id))))]
    [(first res) (pr-str (subs (with-out-str (inspect-map (second res))) 0 250))]))

#_(defn ld
    "Loads the local bindings that have been saved using `save` with `id` as parameter."
    ([id] (ld id nil))
    ([id pos]
     (let [locals (second (if pos
                            (first (filter #(= (key %) pos) (get @saves id)))
                            (last (get @saves id))))]
       (when locals
         (println "Defining:")
         (inspect-map locals))
       (doseq [[sym val] locals]
         (try
           (eval `(def ~(symbol sym) '~val))
           (catch js/Error e (prn sym val) (throw e)))))))

(defn print-saves
  [id]
  (let [locals (take 10 (get @saves id))]
    (doseq [i (reverse (range (count locals)))
            :let [[k v] (first (drop i locals))]]
      (println "Entry id." k)
      (inspect-map v)
      (prn))))








(defonce f-saves (atom {}))  

(defn which-args
  "Takes a list of arg-names, e.g. ([x] [x y] [x y {:keys [a b]}])
  and a number.
  Returns the arg-names that matches n-args."
  [arg-names-list n-args]
  (first
   (filter 
    some?
    (for [arg-names arg-names-list]
      (if (and (some #{'&} arg-names)
               (>= n-args (count (take-while #(not= % '&) arg-names))))
        arg-names
        (when (= n-args (count arg-names))
          arg-names))))))  

(defonce watched-vars (atom {}))

(defn add
  ([x]
   (add x 0))
  ([x y]
   (+ x y))
  ([x y & args]
   (+ x y (apply add args))))  

(defn new*
  [id bindings]
  (swap! miracle.save/saves
         update
         id
         #(into [] (take-last *max-saves* (conj % [(gen-id) bindings]))))
  :ok)

(defn wrapper-fn
  [var f]
  (let [arglists (get @watched-vars var)]
    (if-not arglists
      (throw (js/Error. (str "No meta-data stored for var " var)))
      (fn [& args]
        (let [arglist (first (filter #(= (count %) (count args))
                                     (map #(filter (partial not= '&) %)
                                          arglists)))
              args-vals (into [] (map-indexed (fn [i a] [a (nth args i)]) arglist))]
          (try               
            (let [res (apply f args)]
              (new* var {::ret res, ::args args-vals})
              res)
            (catch js/Error e
              (new* var {::error e, ::args args-vals})
              (throw e))))))))

(comment
  (filter symbol? (keys @miracle.save/saves))
  
  (macroexpand '(store-var-data +))
  (store-var-data +)
  (store-var-data /)
  (store-var-data nth)
  @watched-vars
  (def hehe (wrapper-fn '+ +))
  (def hehe2 (wrapper-fn '/ /))
  (def nn (wrapper-fn 'nth nth))
  
  (time (hehe 1336 334 5 1 2 3 4 5))
  (time (hehe2 1336 334 5 1 2 3 4 0))
  (time (nn [1 2 3] 0))
  (time (nn [1 2 3] 5))
  (print-saves 'nth)
  (time (+ 1336 334 5 1 2 3 4 5))
  
  (print-saves '+)
  
  (pprint (macroexpand '(arglists +)))
  (var-data +)
  (arglists +)
  
  (macroexpand '(store-var-data +))
  
  
  ;; how it looks
  
  (yo 5 10 20)
  
  ;; how I want it to look
  (defn add
    ([x]
     (let [args {'x x}
           ret (add x 0)]
       (save 'add)
       ret))
    ([x y]
     (let [args {'x x
                 'y y}
           res (+ x y)]
       (save 'add)
       res))))

(defn destructure-bindings
  [arg-names values]
  `(let [~(which-args arg-names (count values)) '~values]
     (get-env)))

(comment
  (defn gen-spec-args
    [arg-lists args]
    (let [arg-list (which-args arg-lists (count args))
          [before-and _] (split-with #(not= % '&) arg-list)
          nof-before-and (count before-and)]
      (concat
       (->> (map-indexed
             #(if (coll? %2) (keyword (str "arg-" %1)) %2)
             before-and)
            (#(map vector % args)))
       (when (< nof-before-and (count args))
         [['rest (drop (count before-and) args)]]))))
  
  (defn save-wrapper
    "f-var should be a var or symbol holding a IFn, and f the actual function.
  Returns a function that will save all args and return values when being called.
  The data will be stored in `f-saves`."
    [f-var f args]
    (let [arg-lists (some-> (meta f-var) :arglists)
          ret (apply f args)
          
          args-to-save
          (into {}
                (if arg-lists
                  (->> (eval (destructure-bindings arg-lists args))
                       (remove #(gensym? (name (key %)))))
                  (->> (range (count args))                           ; Generate default arg-names
                       (map #(keyword (str "arg-" (str %))))
                       (#(map vector % args)))))
          
          spec-args  ;; argument specifically for generating specs
          (if arg-lists
            (gen-spec-args arg-lists args)
            (->> (range (count args))                                ; Generate default arg-names
                 (map #(keyword (str "arg-" (str %))))
                 (#(map vector % args))))]
      (new* f-saves
            f-var
            {:args args-to-save
             :spec-args spec-args
             :ret ret})
      ret))

  (defn save-var*
    "Applies `save-wrapper` to a var, which means that whenever that var is called,
  it will save all args and return values of that function.
  Check `save-wrapper` for more information."
    ([ns s]
     (save-var* (ns-resolve ns s)))
    ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)]
       (if (and (ifn? @v) (-> v meta :macro not) (-> v meta ::saved not))
         (let [f @v
               vname (symbol (str ns "/" s))]
           (doto v
             (alter-var-root #(fn [& args] (save-wrapper v % args)))
             (alter-meta! assoc ::saved f)))))))

  (defn unsave-var*
    "The opposite of `save-var*`, restores the var to its original state."
    ([ns s]
     (unsave-var* (ns-resolve ns s)))
    ([v]
     (let [^clojure.lang.Var v (if (var? v) v (resolve v))
           ns (.ns v)
           s  (.sym v)
           f  ((meta v) ::saved)]
       (when f
         (doto v
           (alter-var-root (constantly ((meta v) ::saved)))
           (alter-meta! dissoc ::saved))))))

  (defn save-ns*
    "Applies `save-var*` to all function vars in a namespace."
    [ns]
    (let [ns (the-ns ns)]
      (when-not ('#{clojure.core miracle.save} (.getName ns))
        (let [ns-fns (->> ns ns-interns vals (filter (comp fn? var-get)))]
          (doseq [f ns-fns]
            (save-var* f))))))

  (defn unsave-ns*
    "Applies `unsave-var*` to all function vars in a namespace."
    [ns]
    (let [ns-fns (->> ns the-ns ns-interns vals)]
      (doseq [f ns-fns]
        (unsave-var* f)))))













(comment
  ;; Example usage of basic `save`
  (defn add [x y] (save :a) (+ x y))
  (add 5 10)
  (ld :a)
  x ;; 5
  y ;; 10
  
  (add 20 30)
  (add 7 13)
  
  (print-saves :a))



(comment
  ;; Example usage of `save-var*`
  (do
    (defn yo ([x] x) ([x {:keys [a b]}] (+ x a b)) ([x y & rest] (apply + x y rest)))
    (def wat (partial yo 5))  
    (save-var* 'yo)  
    (save-var* 'wat)  
    (reset! f-saves {})
    (yo 5)
    (wat 5 10)
    (yo 5 {:a 20 :b 30})
    (yo 5 30 40)
    (yo 5 30 40 50)
    @f-saves)
  
  (unsave-var* 'yo))

