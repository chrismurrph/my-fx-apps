(ns om-fx.next.common
  (:require [om-fx.next.protocols :as p]))

(defn component?
  "Returns true if the argument is an Om component."
  [x]
  (if-not (nil? x)
    (or (instance? p/IReactComponent x)
        (satisfies? p/IReactComponent x))
    false))

(defn props [component]
  {:pre [(component? component)]}
  (:omcljs$value (:props component)))

(defn- get-prop
  "PRIVATE: Do not use"
  [c k]
  (get (:props c) k))

(defn path
  "Returns the component's Om data path."
  [c]
  (get-prop c :omcljs$path))

(defprotocol Ident
  (ident [this props] "Return the ident for this component"))

