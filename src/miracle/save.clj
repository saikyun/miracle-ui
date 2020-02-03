(ns miracle.save
  (:require [cljs.analyzer :as cljs]))

(defmacro var-data
  [sym]
  (cljs/resolve-var &env sym))

(defmacro store-var-data
  [sym]
  `(do (swap! miracle.save/watched-vars
              assoc
              '~sym
              ~(:arglists (:meta (cljs/resolve-var &env sym))))
       :ok))

(defmacro arglists
  [sym]
  (:arglists (:meta (cljs/resolve-var &env sym))))

(defmacro get-env
  "Returns the locally defined variables and their values."
  []
  (into {} (for [k (keys (:locals &env))]
             [`'~k k])))  

(defmacro save
  "Used to save all local bindings, takes an identifier as a parameter.
  The identifier is used with `ld` in order to load the local bindings where `save` was called."
  [key]
  `(do (swap! miracle.save/saves
              update
              ~key
              #(into []
                     (take-last 
                      *max-saves*
                      (conj %
                            [(miracle.save/gen-id)
                             (into {} (list ~@(let [ks (keys (:locals &env))]
                                                (for [k ks]
                                                  `['~k ~k]))))]))))
       :ok))

(defmacro save-do
  "Used to save all local bindings, takes an identifier as a parameter.
  The identifier is used with `ld` in order to load the local bindings where `save` was called."
  [key & body]
  `(let [~'_ (swap! miracle.save/saves
                    update
                    ~key
                    #(into []
                           (take-last 
                            *max-saves*
                            (conj %
                                  [(miracle.save/gen-id)
                                   (into {} (list ~@(let [ks (keys (:locals &env))]
                                                      (for [k ks]
                                                        `['~k ~k]))))]))))]
     ~@body))
