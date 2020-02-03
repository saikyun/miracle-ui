(ns example.renderer
  (:require ["react" :as react]
            [reagent.core :as r]
            ["react-split-pane" :as SplitPane]
            ["net" :as net]
            [cljs-bean.core :refer [bean]]
            [example.edn-validator :refer [validate]]
            
            [cljs.reader :as read]
            [clojure.string :as str]
            
            [clojure.repl :refer [doc]]
            [clojure.pprint :refer [pp pprint]]
            [clojure.core.protocols :as p]
            
            [miracle.save :refer-macros [save]]
            [example.eval :refer [eval-str!] :refer-macros [eval! raw-eval!]]))

(defonce c (.connect net #js {:port 50885, :host "localhost"}))

(def buffer (atom ""))
(def validated [])
(def parsed (atom []))

(defonce save-keys (r/atom []))
(defonce current-key (r/atom nil))

(defn parse!
  [parsed]
  (let [vs validated]
    (set! validated [])
    (swap! parsed concat (map read/read-string vs))))

(comment
  (def s "{:command :get-key, :data [[776 {:miracle.save/ret 1, :miracle.save/args {coll [1 2 3], n 0}, :miracle.save/arglist (coll n)}] [777 {:miracle.save/ret 1, :miracle.save/args {coll [1 2 3], n 0}, :miracle.save/arglist (coll n)}] [778 {:miracle.save/error #object[Error Error: No item 5 in vector of length 3], :miracle.save/args {coll [1 2 3], n 5}, :miracle.save/arglist (coll n)}] [783 {:miracle.save/ret 1, :miracle.save/args {coll [1 2 3], n 0}, :miracle.save/arglist [coll n]}] [784 {:miracle.save/error #object[Error Error: No item 5 in vector of length 3], :miracle.save/args {coll [1 2 3], n 5}, :miracle.save/arglist [coll n]}]]}")

  (def s2 "#object[Error Error: No item 5 in vector of length 3]")

  ((partial
    read/read-string 
    {:default (fn [tag v] (str "#" tag v))
     :readers {'object #(str "#object" %)}})
   "(1 2 3 #object[Error])"
   )
  
  (read/read-string 
   {:default (fn [tag v] (str "#" tag v))
    :readers {'object #(str "#object" %)}}
   "#object[\"Error Error: aoe\"]")
  
  
  
  (p/datafy (js/Error. "wat"))
  
  (read/read-string (pr-str (js/Error. "hej")))
  
  (read/read-string (pr-str (js/Error. "wat")))
  
  ((partial
    read/read-string 
    {:default (fn [tag v] (str "#" tag v))
     :readers {'object str}})
   s2
   )



  ((partial
    read/read-string 
    {:default (fn [tag v]
                (save :def-thing-2)
                (str "#" tag v))
     :readers {'Error (fn [v]
                        (save :err-thing)
                        (first v))}})
   (:res (first parsed)))
  
  )


(defn handle-parsed!
  [parsed]
  (save :que-1)
  (doseq [{:keys [command data] :as m}
          (map (partial
                read/read-string 
                {:default (fn [tag v] (str "#" tag v))
                 :readers {'Error (fn [v] (first v))}}) 
               (filter some? (map :res parsed)))]
    (println "parsing!")
    (save :que-m)
    (case command
      :save-keys (reset! save-keys (into [] data))
      :get-key (do (println "GOT KEY!") (reset! current-key data))
      :nope)))

(defn handle-chunk!
  [c]
  (let [s (.toString c)]
    (println ";;=>" 
             (subs s 0 100)
             (when (seq (subs s 100 101))
               (str "\n...\n"
                    (apply str (take-last 100 s)))))
    #_ (println "got chunk" s)
    (swap! buffer
           (fn [buffer]
             (let [buffer (str buffer s)]
               (let [[valid remaining]
                     (loop [{:keys [well-formed remaining]} (validate buffer)
                            res []]
                       (cond
                         (and remaining well-formed)
                         (do
                           (save :well-watter-2)
                           #_(println "is well AND remaining!")
                           (recur (validate remaining) (conj res well-formed)))
                         
                         well-formed
                         (do
                           #_(println "is well!")
                           [(conj res well-formed) remaining])
                         
                         :else
                         (do
                           #_(println "not well formed")
                           [res remaining])))]
                 (set! validated (concat validated valid))
                 remaining))))
    (parse! parsed)
    (try
      (handle-parsed! @parsed)    
      (catch js/Error e
        (reset! parsed [])
        (println "ERROR")
        (save :err-pars)
        (throw e)))
    (reset! parsed [])))

(comment
  (repeat 10000 "hej")
  
  (macroexpand '(eval! c (+ 1 1)))
  
  (eval! c {:a "yee"})
  
  (eval! c *ns*)
  
  (tap> (with-out-str (miracle.save/print-saves :yee)))
  
  (eval! )
  
  (do
    )  
  
  (pprint parsed)
  (pprint validated)
  (pprint buffer)
  
  
  )

(defn get-keys!
  []
  (eval! c {:command :save-keys, :data (filter symbol? (keys @miracle.save/saves))}))

(defn show-saves!
  [k]
  (save :yee)
  (eval-str! c (str "{:command :get-key, :data (get @miracle.save/saves " 
                    (if (symbol? k)
                      (str "'" k)
                      k) ")}")))

(defonce selected-date (r/atom nil))

(defn left
  []
  [:div
   [:button {:on-click get-keys!}
    "Get keys"]
   [:ul
    (doall (map (fn [k] [:li {:key k} [:button {:on-click #(show-saves! k)} k]]) @save-keys))]])

(defn right
  [filters]
  (let [headers (sort (distinct (mapcat (comp keys second) (take 100 @current-key))))
        cols
        (filter
         (fn [[_ data]]
           (reduce
            (fn [acc [filter-k f]]
              (let [f (cond
                        (seq f) #(str/includes? % f)
                        :else nil)]
                (and acc (or (nil? f)
                             (some-> 
                              (get data filter-k)
                              f)))))
            true
            @filters))
         (map
          (fn [[k v]]
            [k (into {} (map (fn [[i-k i-v]] [i-k (str i-v)]) v))])
          @current-key))]
    (save :yehtue-2)
    [:div [:table
           [:thead
            [:tr
             [:td "id"]
             (doall (map (fn [k] [:td {:key k} (str "- " k " -")])
                         headers))]]
           [:tbody
            [:tr
             [:td {:key "k"}]
             (doall (map 
                     (fn [k]
                       [:td 
                        {:key k}
                        [:input {:type "text"
                                 :value (get @filters k)
                                 :on-change #(swap! filters assoc k (.. % -target -value))}]])
                     headers))]]
           (doall (map 
                   (fn [[row-id data]]
                     [:tr
                      {:key row-id}
                      [:td row-id]
                      (doall 
                       (map
                        (fn [k] [:td {:key k} (some-> (get data k) (subs 0 100))])
                        headers))])
                   cols))]]))

(defn both-panes []
  [(r/adapt-react-class SplitPane)
   {:minSize 300}
   [left]
   [right (r/atom {})]])

(defn start []
  (js/console.log "renderer - start")
  (r/render [both-panes]
            (.getElementById js/document "app")))

(defn init []
  (js/console.log "renderer - init")
  (.on c "data" (fn [chunk] (handle-chunk! chunk)))
  (raw-eval! c :cljs/quit)
  #_  (eval! c :cljs/quit)
  #_    (eval! c (shadow/repl :renderer))
  (start))

(defn stop []
  (js/console.log "renderer - stop"))
