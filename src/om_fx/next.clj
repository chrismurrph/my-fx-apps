(ns om-fx.next
  (:require
    [om-fx.next.protocols :as p]
    [clojure.string :as str]
    [om-fx.next.reconciler :as rec]
    [om-fx.next.common :as com]
    [om-fx.next.treedb :as db])
  (:import [java.io Writer]
           (clojure.lang IMapEntry IObj)
           (om_fx.next.reconciler Reconciler)))

(def props com/props)
(def factory rec/factory)

(def ^:private roots (atom {}))
(def ^{:dynamic true} *raf* nil)

(defn remove-root!
  "Remove a root target (a DOM element) from a reconciler. The reconciler will
   no longer attempt to reconcile application state with the specified root."
  [reconciler target]
  (p/remove-root! reconciler target))

(defn add-root!
  "Given a root component class and a target root DOM node, instantiate and
   render the root class using the reconciler's :state property. The reconciler
   will continue to observe changes to :state and keep the target node in sync.
   Note a reconciler may have only one root. If invoked on a reconciler with an
   existing root, the new root will replace the old one."
  ([reconciler root-class target]
   (add-root! reconciler root-class target nil))
  ([reconciler root-class target options]
   {:pre [(db/reconciler? reconciler) (fn? root-class)]}
   (when-let [old-reconciler (get @roots target)]
     (remove-root! old-reconciler target))
   (swap! roots assoc target reconciler)
   (p/add-root! reconciler root-class target options)))

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
           (binding [om-fx.next.reconciler/*reconciler* (db/get-reconciler this#)
                     om-fx.next.reconciler/*depth* (inc (om-fx.next/depth this#))
                     om-fx.next.reconciler/*shared* (om-fx.next/shared this#)
                     om-fx.next.reconciler/*instrument* (om-fx.next/instrument this#)
                     om-fx.next.reconciler/*parent* this#]
             (let [ret# (do ~@body)
                   props# (:props this#)]
               (when-not @(:omcljs$mounted? props#)
                 (swap! (:omcljs$mounted? props#) not))
               ret#)))))
    'componentWillMount
    (fn [[name [this :as args] & body]]
      `(~name [this#]
         (let [~this this#
               indexer# (get-in (db/get-reconciler this#) [:config :indexer])]
           (when-not (nil? indexer#)
             (om-fx.next.protocols/index-component! indexer# this#))
           ~@body)))}
   :defaults
   `{~'initLocalState
     ([this#])
     ~'componentWillMount
     ([this#]
       (let [indexer# (get-in (db/get-reconciler this#) [:config :indexer])]
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

(defn instrument [component]
  {:pre [(com/component? component)]}
  (com/get-prop component :omcljs$instrument))

(defn shared
  "Return the global shared properties of the Om Next root. See :shared and
   :shared-fn reconciler options."
  ([component]
   (shared component []))
  ([component k-or-ks]
   {:pre [(com/component? component)]}
   (let [shared (com/get-prop component :omcljs$shared)
         ks     (cond-> k-or-ks
                        (not (sequential? k-or-ks)) vector)]
     (cond-> shared
             (not (empty? ks)) (get-in ks)))))

(defn depth
  "PRIVATE: Returns the render depth (a integer) of the component relative to
   the mount root."
  [component]
  (when (com/component? component)
    (com/get-prop component :omcljs$depth)))

(defn reconciler
  "Construct a reconciler from a configuration map.

   Required parameters:
     :state        - the application state. If IAtom value is not supplied the
                     data will be normalized into the default database format
                     using the root query. This can be disabled by explicitly
                     setting the optional :normalize parameter to false.
     :parser       - the parser to be used

   Optional parameters:
     :shared       - a map of global shared properties for the component tree.
     :shared-fn    - a function to compute global shared properties from the root props.
                     the result is merged with :shared.
     :send         - required only if the parser will return a non-empty value when
                     run against the supplied :remotes. send is a function of two
                     arguments, the map of remote expressions keyed by remote target
                     and a callback which should be invoked with the result from each
                     remote target. Note this means the callback can be invoked
                     multiple times to support parallel fetching and incremental
                     loading if desired. The callback should take the response as the
                     first argument and the the query that was sent as the second
                     argument.
     :normalize    - whether the state should be normalized. If true it is assumed
                     all novelty introduced into the system will also need
                     normalization.
     :remotes      - a vector of keywords representing remote services which can
                     evaluate query expressions. Defaults to [:remote]
     :root-render  - the root render function. Defaults to ReactDOM.render
     :root-unmount - the root unmount function. Defaults to
                     ReactDOM.unmountComponentAtNode
     :logger       - supply a goog.log compatible logger
     :tx-listen    - a function of 2 arguments that will listen to transactions.
                     The first argument is the parser's env map also containing
                     the old and new state. The second argument is a map containing
                     the transaction, its result and the remote sends that the
                     transaction originated."
  [{:keys [state shared shared-fn
           parser indexer
           ui->props normalize
           send merge-sends remotes
           merge merge-tree merge-ident
           prune-tree
           optimize
           history
           root-render root-unmount
           pathopt
           migrate id-key
           instrument tx-listen
           easy-reads]
    :or   {ui->props    rec/default-ui->props
           indexer      rec/indexer
           merge-sends  #(merge-with into %1 %2)
           remotes      [:remote]
           merge        rec/default-merge
           merge-tree   rec/default-merge-tree
           merge-ident  rec/default-merge-ident
           prune-tree   rec/default-extract-errors
           optimize     (fn [cs] (sort-by depth cs))
           history      100
           root-render  (fn [c target]
                          (println (str "Need do root render of " c " onto " target))
                          c)
           root-unmount (fn [x])
           pathopt      false
           migrate      rec/default-migrate
           easy-reads   true}
    :as   config}]
  {:pre [(map? config)]}
  (let [idxr   (indexer)
        norm?  (instance? clojure.lang.Atom state)
        state' (if norm? state (atom state))
        logger (when (contains? config :logger)
                 (:logger config))
        ret    (Reconciler.
                 {:state      state' :shared shared :shared-fn shared-fn
                  :parser     parser :indexer idxr
                  :ui->props  ui->props
                  :send       send :merge-sends merge-sends :remotes remotes
                  :merge      merge :merge-tree merge-tree :merge-ident merge-ident
                  :prune-tree prune-tree
                  :optimize   optimize
                  :normalize  (or (not norm?) normalize)
                  :history    []
                              :root-render root-render :root-unmount root-unmount
                              :logger logger :pathopt pathopt
                              :migrate migrate :id-key id-key
                              :instrument instrument :tx-listen tx-listen
                              :easy-reads easy-reads}
                 (atom {:queue []
                        :remote-queue {}
                        :queued false :queued-sends {}
                        :sends-queued false
                        :target nil :root nil :render nil :remove nil
                        :t 0 :normalized norm?}))]
    ret))