(ns om-fx.next
  (:require
    [om-fx.next.protocols :as p])
  (:import [java.io Writer]))

(defn collect-statics [dt]
  (letfn [(split-on-static [forms]
            (split-with (complement '#{static}) forms))
          (split-on-symbol [forms]
            (split-with (complement symbol?) forms))]
    (loop [dt (seq dt) dt' [] statics {:fields {} :protocols []}]
      (if dt
        (let [[pre [_ sym & remaining :as post]] (split-on-static dt)
              dt' (into dt' pre)]
          (if (seq post)
            (cond
              (= sym 'field)
              (let [[field-info dt] (split-at 2 remaining)]
                (recur (seq dt) dt'
                       (update-in statics [:fields] conj (vec field-info))))
              (symbol? sym)
              (let [[protocol-info dt] (split-on-symbol remaining)]
                (recur (seq dt) dt'
                       (update-in statics [:protocols]
                                  into (concat [sym] protocol-info))))
              :else (throw (IllegalArgumentException. "Malformed static")))
            (recur nil dt' statics)))
        {:dt dt' :statics statics}))))

(def lifecycle-sigs
  '{initLocalState [this]
    shouldComponentUpdate [this next-props next-state]
    componentWillReceiveProps [this next-props]
    componentWillUpdate [this next-props next-state]
    componentDidUpdate [this prev-props prev-state]
    componentWillMount [this]
    componentDidMount [this]
    componentWillUnmount [this]
    render [this]})

(defn validate-sig [[name sig :as method]]
  (let [sig' (get lifecycle-sigs name)]
    (assert (= (count sig') (count sig))
            (str "Invalid signature for " name " got " sig ", need " sig'))))

(def reshape-map-clj
  {:reshape
   {'render
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this this#]
           (binding [om.next/*reconciler* (om-fx.next/get-reconciler this#)
                     om.next/*depth*      (inc (om-fx.next/depth this#))
                     om.next/*shared*     (om-fx.next/shared this#)
                     om.next/*instrument* (om-fx.next/instrument this#)
                     om.next/*parent*     this#]
             (let [ret# (do ~@body)
                   props# (:props this#)]
               (when-not @(:omcljs$mounted? props#)
                 (swap! (:omcljs$mounted? props#) not))
               ret#)))))
    'componentWillMount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this    this#
               indexer# (get-in (om-fx.next/get-reconciler this#) [:config :indexer])]
           (when-not (nil? indexer#)
             (om-fx.next.protocols/index-component! indexer# this#))
           ~@body)))}
   :defaults
   `{~'initLocalState
     ([this#])
     ~'componentWillMount
     ([this#]
       (let [indexer# (get-in (om-fx.next/get-reconciler this#) [:config :indexer])]
         (when-not (nil? indexer#)
           (om-fx.next.protocols/index-component! indexer# this#))))
     ~'render
     ([this#])}})

(defn reshape [dt {:keys [reshape defaults]}]
  (letfn [(reshape* [x]
            (if (and (sequential? x)
                     (contains? reshape (first x)))
              (let [reshapef (get reshape (first x))]
                (validate-sig x)
                (reshapef x))
              x))
          (add-defaults-step [ret [name impl]]
            (if-not (some #{name} (map first (filter seq? ret)))
              (let [[before [p & after]] (split-with (complement '#{Object}) ret)]
                (into (conj (vec before) p (cons name impl)) after))
              ret))
          (add-defaults [dt]
            (reduce add-defaults-step dt defaults))
          (add-object-protocol [dt]
            (if-not (some '#{Object} dt)
              (conj dt 'Object)
              dt))]
    (->> dt (map reshape*) vec add-object-protocol add-defaults)))

(defn defui*-clj [name forms]
  (let [docstring (when (string? (first forms))
                    (first forms))
        forms (cond-> forms
                      docstring rest)
        {:keys [dt statics]} (collect-statics forms)
        [other-protocols obj-dt] (split-with (complement '#{Object}) dt)
        klass-name (symbol (str name "_klass"))
        class-methods (when-not (empty? (:protocols statics))
                        (->> (partition 2 (:protocols statics))
                             (reduce
                               (fn [r [_ impl]]
                                 (assoc r (keyword (first impl))
                                          (cons 'fn (rest impl)))) {:params '(fn [this])})))]
    `(do
       (declare ~name)
       (defrecord ~klass-name [~'state ~'refs ~'props ~'children]
         ;; TODO: non-lifecycle methods defined in the JS prototype - António
         om-fx.next.protocols/IReactLifecycle
         ~@(rest (reshape obj-dt reshape-map-clj))

         ~@other-protocols

         ~@(:protocols statics)

         om-fx.next.protocols/IReactComponent
         (~'-render [this#]
           (p/componentWillMount this#)
           (p/render this#)))
       (defmethod clojure.core/print-method ~(symbol (str (munge *ns*) "." klass-name))
         [o# ^Writer w#]
         (.write w# (str "#object[" (ns-name *ns*) "/" ~(str name) "]")))
       (let [c# (fn ~name [state# refs# props# children#]
                  (~(symbol (str (munge *ns*) "." klass-name ".")) state# refs# props# children#))]
         (def ~(with-meta name
                          (merge (meta name)
                                 (when docstring
                                   {:doc docstring})))
           (with-meta c#
                      (merge {:component c#
                              :component-ns (ns-name *ns*)
                              :component-name ~(str name)}
                             ~class-methods)))))))

(defmacro defui [name & forms]
  (defui*-clj name forms))

(defn- get-prop
  "PRIVATE: Do not use"
  [c k]
  (get (:props c) k))

(defn component?
  "Returns true if the argument is an Om component."
  [x]
  (if-not (nil? x)
    (or (instance? p/IReactComponent x)
       (satisfies? p/IReactComponent x))
    false))

(defn instrument [component]
  {:pre [(component? component)]}
  (get-prop component :omcljs$instrument))

(defn shared
  "Return the global shared properties of the Om Next root. See :shared and
   :shared-fn reconciler options."
  ([component]
   (shared component []))
  ([component k-or-ks]
   {:pre [(component? component)]}
   (let [shared (get-prop component :omcljs$shared)
         ks     (cond-> k-or-ks
                        (not (sequential? k-or-ks)) vector)]
     (cond-> shared
             (not (empty? ks)) (get-in ks)))))

(defn depth
  "PRIVATE: Returns the render depth (a integer) of the component relative to
   the mount root."
  [component]
  (when (component? component)
    (get-prop component :omcljs$depth)))

(defn get-reconciler
  [c]
  {:pre [(component? c)]}
  (get-prop c :omcljs$reconciler))
