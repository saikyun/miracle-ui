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

(defonce c (.connect net #js {:port 51414 :host "localhost"}))

(defonce timer (atom nil))

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

(def last-res (r/atom nil))

(comment

  {:command :bounce, :data (watch-ns-form 'test-stuff.core)}
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
    (save :que-m)
    (case command
      :save-keys (reset! save-keys (into [] data))
      :bounce (eval-str! c (pr-str data))
      :get-key (reset! current-key data)
      (reset! last-res m))))

(defn handle-chunk!
  [c]
  (let [s (.toString c)]
    #_ (println ";;=>" 
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
  (let [k-str (if (symbol? k)
                (str "'" k)
                k)]
    (eval-str! c (str "{:command :get-key, :data {:key " k-str ", :vals (get @miracle.save/saves " 
                      k-str ")}}"))))

(defn refresh-current-key!
  []
  (when-let [k (:key @current-key)]
    (show-saves! k)))

(defn refresh!
  []
  (refresh-current-key!)
  (get-keys!)
  
  (when-let [t @timer]
    (js/clearTimeout t))  
  
  (reset! timer (js/setTimeout refresh! 1000)))

(defn key-button
  [k]
  [:li {:key k}
   [:button {:on-click #(show-saves! k)
             :class "button"} (str k)]])

(defn left
  []
  [:div
   [:button {:on-click get-keys!
             :class "button"}
    "Get keys"]
   [:ul
    (doall (map key-button @save-keys))]])

(def context (r/atom nil))
(add-watch 
 context 
 :set-miracle-context
 (fn [_ _ _ {:keys [key id]}]
   (let [key-str (if (symbol? key)
                   (str "'" key)
                   (str key))]
     (eval-str! c 
                (str
                 "(miracle.save/set-context! {:key " key-str ",
:id " id "})")))))

(defn inspect-row
  [current-key headers [row-id data]]
  (let [inspecting-k (:key current-key)]
    [:tr
     {:key row-id
      :class 
      (str "selectable"
           (when (and (= inspecting-k (:key @context))
                      (= row-id (:id @context)))
             " is-selected"))
      :on-click #(reset! context {:key inspecting-k
                                  :id row-id})}
     [:td row-id]
     (doall 
      (map
       (fn [k] [:td {:key k} (some-> (get data k) (subs 0 100))])
       headers))]))

(defn right
  [filters]
  (let [inspecting-k (:key @current-key)
        massaged (map (fn [[k v]] [k (assoc (:miracle.save/args v)
                                            :miracle.save/ret
                                            (:miracle.save/ret v)
                                            
                                            :miracle.save/error
                                            (:miracle.save/error v))]) (:vals @current-key))
        headers (into []
                      (sort-by
                       str
                       (distinct 
                        (mapcat 
                         (comp keys second) 
                         (take 100 massaged)))))
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
          massaged))]
    [:div {:class "container"}
     [:h2 (str "inspecting `" inspecting-k "`")]
     [:table {:class "table"}
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
                            :class "input"
                            :value (get @filters k)
                            :on-change #(swap! filters assoc k (.. % -target -value))}]])
                headers))]
       (doall (map (partial inspect-row
                            @current-key
                            headers)
                   cols))]]]))

(defn both-panes []
  [:> SplitPane
   {:minSize 300}
   [left]
   [right (r/atom {})]])

(def eval-s (r/atom ""))

(defn submit-eval
  [ev]
  (.preventDefault ev)
  (eval-str! c @eval-s))

(def watch-s (r/atom ""))

(defn watch-ns
  [ev]
  (.preventDefault ev) (eval-str! c (str "{:command :bounce, :data {:command :bounce, :data (watch-ns-form '" @watch-s ")}}")))

(defn main
  []
  [:<>
   [:p "Selected context: " @context]
   (comment
     [:form {:on-submit submit-eval}
      [:input {:type "text"
               :class "input"
               :value @eval-s
               :on-change #(reset! eval-s (.. % -target -value))}]
      [:button {:type "submit"
                :class "button"} "Eval"]]
     [:p "Last res: " @last-res])
   
   [:form {:on-submit watch-ns}
    [:input {:type "text"
             :class "input"
             :value @watch-s
             :on-change #(reset! watch-s (.. % -target -value))}]
    [:button {:type "submit"
              :class "button"} "Watch ns"]]
   [both-panes]])

(defn start []
  (js/console.log "renderer - start")
  (refresh!)
  (get-keys!)
  (r/render [main]
            (.getElementById js/document "app")))

(defn init []
  (js/console.log "renderer - init")
  (.on c "data" (fn [chunk] (handle-chunk! chunk)))
  (raw-eval! c :cljs/quit)
  #_  (eval! c :cljs/quit)
  (eval! c (shadow/repl :test-stuff))
  (start))

(defn stop []
  (js/console.log "renderer - stop"))
