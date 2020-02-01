(ns example.edn-validator
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [cljs.reader :as reader]
            [miracle.save :refer-macros [save]]))

;; Cases:
;; lists: {} () []
;; strings: ""
;; comments: ;; \n (doesn't have to be closed)
;; escape: \ (ignores next)

(comment
  ;; model
  {:stack []
   :in-string false
   :in-comment false
   :escape-next false}
  
  ;; example
  {:stack ["}" "}"]
   :in-string true
   :in-comment false
   :escape-next false})

(def lists
  {"{" "}"
   "[" "]"
   "(" ")"})

(def list-closers (into #{} (vals lists)))

(defn validate
  [s]
  (loop [to-validate s
         state {:validated ""
                :stack []
                :in-string false
                :in-comment false
                :escape-next false}]
    (let [c       (subs to-validate 0 1)
          cs      (subs to-validate 1)
          push-c #(update % :validated str c)]
      
      (cond
        (= c "")        
        (if (or (seq (:stack state))
                (:in-string state)
                (:escape-next state))
          {:error "String ended in unfinished state."
           :remaining cs
           :state state}
          {:well-formed (:validated state)})
        
        (:escape-next state)        
        (recur cs (-> state
                      (assoc :escape-next false)
                      push-c))        
        
        (:in-comment state)                
        (if (= c "\n")
          (recur cs (-> state
                        (assoc :in-comment false)
                        push-c))
          (recur cs (push-c state)))                        
        
        (= c "\\")                
        (recur cs (-> state
                      (assoc :escape-next true)
                      push-c))                
        
        (= c "\"")        
        (recur cs (-> state
                      (update :in-string not)
                      push-c))                
        
        (:in-string state)                        
        (recur cs (push-c state))
        
        (= c ";")        
        (recur cs (-> state
                      (assoc :in-comment true)
                      push-c))        
        
        (lists c)
        (recur cs (-> state
                      (update :stack conj (lists c))
                      push-c))        
        
        (list-closers c)
        (if (= c (last (:stack state)))
          (if (= 1 (count (:stack state)))
            (let [state (-> state
                            (update :stack pop)
                            push-c)]
              {:well-formed (:validated state)
               :remaining cs})
            (recur cs (-> state
                          (update :stack pop)
                          push-c)))
          {:error (if (seq (:stack state))
                    "Closing wrong kind of list."
                    "Closing list though there are no open lists.")
           :state (push-c state)
           :remaining cs})        
        
        :else (recur cs (push-c state)))))
  )

(comment
  (validate "{:a 10}") ;; good
  (validate "{:a [1 2 3}") ;; bad
  (validate "{:a [1 2 3]}") ;;good
  
  (validate "{:a [;;1 2 3]}") ;; bad
  (validate "{:a [;;1 2\n 3]}") ;; good
  
  (validate "{:a \"yo\"}") ;; good
  (validate "{:a \"[;;1 2 3]\"}") ;; good
  
  (validate "{:a \"[;;1 \n\"\\\"2\\\"\" 3]\"}") ;; good
  (validate "{:a \"[;;1 \n\"\\\"2\\\"\" 3]\"
:b \"hej\"}") ;; good
  (validate "{:a \"[;;1 \n\"\\\"2\\\"\" 3]\"
;;}") ;; bad
  
  (validate "{:a \"[;;1 \n\"\\\"2\\\"\" 3]\"
;;}
}") ;; good
  

  ;; example of parsing one form at the time
  (validate "{:a 10}\n{:b") ;; good, can run `validate` again, like so:
  (validate (:remaining (validate "{:a 10}\n{:b"))) ;; bad
  
  )
