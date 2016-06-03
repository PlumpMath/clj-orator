(ns clj-orator.core
  (:require [com.stuartsierra.component :as comp]
            [clojure.core.async :refer [chan <! <!! take! >! >!! put! go go-loop thread]]))

(defmacro build-defrecord [name f]
  (let [logprefix (str "[" name "] ")]
    `(defrecord ~name [~'ch-in ~'ch-out]

       comp/Lifecycle

       (start [this]
         (print ~(str logprefix "starting..."))
         (go-loop [msg# (<! ~'ch-in)]
           (when-not (= msg# ::stopstopstop)
             (println ~(str logprefix "got message:") msg#)
             (put! ~'ch-out (~f msg#))
             (recur (<! ~'ch-in))))
         (println "done.")
         this)

       (stop [this]
         (print ~(str logprefix "stopping..."))
         (put! ~'ch-in ::stopstopstop)
         (println "done.")))))

(defmacro build-ctor [name]
  (let [mapsym (symbol (str "map->" name))]
    `(defn ~'new-foo [~'ch-in ~'ch-out]
       (~mapsym {:ch-in ~'ch-in
                 :ch-out ~'ch-out}))))

(defmacro defcomponent [name f]
  `(do (build-defrecord name f)
       (build-ctor name)))
