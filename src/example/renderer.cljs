(ns example.renderer
  (:require ["react" :as react]
            [reagent.core :as r]
            ["react-split-pane" :as SplitPane]
            ["net" :as net]
            [cljs-bean.core :refer [bean]]
            [example.edn-validator :refer [validate]]
            
            [cljs.reader :as read]
            
            [clojure.repl :refer [doc]]
            [clojure.pprint :refer [pp pprint]]            
            [miracle.save :refer-macros [save]]))

(def c (.connect net #js {:port 5555, :host "localhost"}))

(def buffer "")
(def validated [])
(def parsed (atom []))

(defonce save-keys (r/atom []))
(defonce current-key (r/atom nil))

(defn parse!
  [parsed]
  (let [vs validated]
    (set! validated [])
    (swap! parsed concat (map read/read-string vs))))

(defn handle-parsed!
  [parsed]
  (doseq [{:keys [command data] :as m} (map read/read-string (filter some? (map :val parsed)))]
    (println m)
    (save :que-m)
    (case command
      :save-keys (reset! save-keys (into [] data))
      :get-key (reset! current-key data)
      :nope)))

(defn handle-chunk!
  [c]
  (println "got chunk" c)
  (set! buffer (str buffer (.toString c)))
  (let [[valid remaining]
        (loop [{:keys [well-formed remaining]} (validate buffer)
               res []]
          (if (and remaining well-formed)
            (recur (validate remaining) (conj res well-formed))
            [res remaining]))]
    (set! buffer remaining)
    (set! validated (concat validated valid)))
  (parse! parsed)  
  (handle-parsed! @parsed)    
  (reset! parsed []))

(comment
  (repeat 10000 "hej")
  
  (tap> (with-out-str (miracle.save/print-saves :yee)))
  
  (do
    )  
  
  (pprint parsed)
  (pprint validated)
  (pprint buffer)
  
  
  )

(defn get-keys!
  []
  (.write c "{:command :save-keys, :data (keys @miracle.save/saves2)}"))

(defn show-saves!
  [k]
  (save :yee)
  (.write c (str "{:command :get-key, :data (get @miracle.save/saves2 " k ")}")))

(defonce selected-date (r/atom nil))

(defn left
  []
  [:div
   [:button {:on-click get-keys!}
    "Get keys"]
   [:ul
    (doall (map (fn [k] [:li {:key k} [:button {:on-click #(show-saves! k)} k]]) @save-keys))]])

(defn both-panes []
  [(r/adapt-react-class SplitPane)
   {:minSize 300}
   [left]
   [:div (str @current-key)]])

(defn start []
  (js/console.log "renderer - start")
  (r/render [both-panes]
            (.getElementById js/document "app")))

(defn init []
  (js/console.log "renderer - init")
  (.on c "data" (fn [chunk] (handle-chunk! chunk)))
  (start))

(defn stop []
  (js/console.log "renderer - stop"))
