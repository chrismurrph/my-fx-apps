(ns om-fx.next.reconciler
  (:require [om-fx.next.protocols :as p]
            [om-fx.util :as util]
            [om-fx.next.common :as com]
            [om-fx.next.treedb :as db]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.reflect :as reflect]
            [om-fx.next.impl.parser :as parser]
            [om-fx.next.treedb :as treedb])
  (:import (om_fx.next.common Ident)
           (clojure.lang IDeref)
           (om_fx.next.treedb IQuery)))

(defn- recursive-class-path?
  "Returns true if a component's classpath is recursive"
  [c]
  {:pre [(com/component? c)]}
  (not (apply distinct? (treedb/raw-class-path c))))

(defn- get-dispatch-key [prop]
  (cond-> prop
          (or (not (util/ident? prop))
              (= (second prop) '_))
          ((comp :dispatch-key parser/expr->ast))))

(defn full-query
  "Returns the absolute query for a given component, not relative like
   om.next/get-query."
  ([component]
   (when (com/iquery? component)
     (if (nil? (com/path component))
       (replace
         (first
           (get-in @(-> component db/get-reconciler db/get-indexer)
                   [:class-path->query (treedb/class-path component)]))
         (db/get-query component))
       (full-query component (db/get-query component)))))
  ([component query]
   (when (com/iquery? component)
     (let [xf    (cond->> (remove number?)
                          (recursive-class-path? component) (comp (distinct)))
           path' (into [] xf (com/path component))
           cp    (treedb/class-path component)
           qs    (get-in @(-> component db/get-reconciler db/get-indexer)
                         [:class-path->query cp])]
       (if-not (empty? qs)
         ;; handle case where child appears multiple times at same class-path
         ;; but with different queries
         (let [q (->> qs
                      (filter #(= path'
                                  (mapv get-dispatch-key
                                        (-> % zip/root (treedb/focus->path path')))))
                      first)]
           (if-not (nil? q)
             (replace q query)
             (throw
               (ex-info (str "No queries exist for component path " cp " or data path " path')
                        {:type :om.next/no-queries}))))
         (throw
           (ex-info (str "No queries exist for component path " cp)
                    {:type :om.next/no-queries})))))))

(defn default-ui->props
  [{:keys [parser pathopt] :as env} c]
  (let [ui (when (and pathopt (satisfies? Ident c) (com/iquery? c))
             (let [id (com/ident c (com/props c))]
               (get (parser env [{id (db/get-query c)}]) id)))]
    (if-not (nil? ui)
      ui
      (let [fq (full-query c)]
        (when-not (nil? fq)
          (let [s  (System/currentTimeMillis)
                ui (parser env fq)
                e  (System/currentTimeMillis)]
            (get-in ui (com/path c))))))))

;; TODO: #?:clj invariant - António
(defn invariant*
  [condition message env]
  #_(let [opts (ana-api/get-options)
        fn-scope (:fn-scope env)
        fn-name (some-> fn-scope first :name str)]
    (when-not (:elide-asserts opts)
      `(let [l# om.next/*logger*]
         (when-not ~condition
           (goog.log/error l#
                           (str "Invariant Violation"
                                (when-not (nil? ~fn-name)
                                  (str " (in function: `" ~fn-name "`)"))
                                ": " ~message)))))))

(defmacro invariant
  [condition message]
  (when (boolean (:ns &env))
    (invariant* condition message &env)))

(defn- cascade-query
  "Cascades a query up the classpath. class-path->query is a map of classpaths
   to their queries. classpath is the classpath where we start cascading (typically
   the direct parent's classpath of the component changing query). data-path is
   the data path in the classpath's query to the new query. new-query is the
   query to be applied to the classpaths. union-keys are any keys into union
   queries found during index building; they are used to access union queries
   when cascading the classpath, and to generate the classpaths' rendered-paths,
   which skip these keys."
  [class-path->query classpath data-path new-query union-keys]
  (loop [cp classpath
         data-path data-path
         new-query new-query
         ret {}]
    (if-not (empty? cp)
      (let [rendered-data-path (into [] (remove (set union-keys)) data-path)
            filter-data-path (cond-> rendered-data-path
                                     (not (empty? rendered-data-path)) pop)
            qs (filter #(= filter-data-path
                           (-> % zip/root (treedb/focus->path filter-data-path)))
                       (get class-path->query cp))
            qs' (into #{}
                      (map (fn [q]
                             (let [new-query (if (or (map? (zip/node q))
                                                     (some #{(peek data-path)} union-keys))
                                               (let [union-key (peek data-path)]
                                                 (-> (treedb/query-template (zip/root q)
                                                                     rendered-data-path)
                                                     zip/node
                                                     (assoc union-key new-query)))
                                               new-query)]
                               (-> (zip/root q)
                                   (treedb/query-template rendered-data-path)
                                   (replace new-query)
                                   (treedb/focus-query filter-data-path)
                                   (treedb/query-template filter-data-path)))))
                      qs)]
        (recur (pop cp) (pop data-path)
               (-> qs' first zip/node) (assoc ret cp qs')))
      ret)))

(defrecord Indexer [indexes extfs]
  IDeref
  (deref [_] @indexes)

  p/IIndexer
  (index-root [_ x]
    (let [prop->classes     (atom {})
          class-path->query (atom {})
          rootq             (db/get-query x)
          root-class        (cond-> x (com/component? x) treedb/react-type)]
      (letfn [(build-index* [class query path classpath union-expr union-keys]
                (invariant (or (not (com/iquery? class))
                               (and (com/iquery? class)
                                    (not (empty? query))))
                           (str "`IQuery` implementation must return a non-empty query."
                                " Check the `IQuery` implementation of component `"
                                (if (com/component? class)
                                  (.. class -constructor -displayName)
                                  (.. class -prototype -constructor -displayName)) "`."))
                (let [recursive? (some #{class} classpath)
                      classpath  (cond-> classpath
                                         (and (not (nil? class))
                                              (not recursive?))
                                         (conj class))
                      dp->cs     (get-in @indexes [:data-path->components])]
                  (when class
                    ;; path could have changed when setting queries, so we only use
                    ;; rootq on the first call (when there's no class-path->query
                    ;; information) - António
                    (let [root-query (if (empty? path)
                                       rootq
                                       (-> @class-path->query
                                           (get [root-class]) first zip/root))]
                      (swap! class-path->query update-in [classpath] (fnil conj #{})
                             (treedb/query-template (treedb/focus-query root-query path) path))))
                  (let [recursive-join? (and recursive?
                                             (some (fn [e]
                                                     (and (util/join? e)
                                                          (not (util/recursion?
                                                                 (util/join-value e)))))
                                                   query)
                                             (= (distinct path) path))
                        recursive-union? (and recursive?
                                              union-expr
                                              (= (distinct path) path))]
                    (when (or (not recursive?)
                              recursive-join?
                              recursive-union?)
                      (cond
                        (vector? query)
                        (let [{props false joins true} (group-by util/join? query)]
                          (swap! prop->classes
                                 #(merge-with into %
                                              (zipmap
                                                (map get-dispatch-key props)
                                                (repeat #{class}))))
                          (doseq [join joins]
                            (let [[prop query']  (util/join-entry join)
                                  prop-dispatch-key (get-dispatch-key prop)
                                  recursion?     (util/recursion? query')
                                  union-recursion? (and recursion? union-expr)
                                  query'         (if recursion?
                                                   (if-not (nil? union-expr)
                                                     union-expr
                                                     query)
                                                   query')
                                  path'          (conj path prop)
                                  rendered-path' (into [] (remove (set union-keys) path'))
                                  cs (get dp->cs rendered-path')
                                  cascade-query? (and (= (count cs) 1)
                                                      (= (-> query' meta :component)
                                                         (treedb/react-type (first cs)))
                                                      (not (map? query')))
                                  query''        (if cascade-query?
                                                   (db/get-query (first cs))
                                                   query')]
                              (swap! prop->classes
                                     #(merge-with into % {prop-dispatch-key #{class}}))
                              (when (and cascade-query? (not= query' query''))
                                (let [cp->q' (cascade-query @class-path->query classpath
                                                            path' query'' union-keys)]
                                  (swap! class-path->query merge cp->q')))
                              (let [class' (-> query'' meta :component)]
                                (when-not (and recursion? (nil? class'))
                                  (build-index* class' query''
                                                path' classpath (if recursion? union-expr nil) union-keys))))))

                        ;; Union query case
                        (map? query)
                        (doseq [[prop query'] query]
                          (let [path'          (conj path prop)
                                class'         (-> query' meta :component)
                                cs             (filter #(= class' (treedb/react-type %))
                                                       (get dp->cs path))
                                cascade-query? (and class' (= (count cs) 1))
                                query''        (if cascade-query?
                                                 (db/get-query (first cs))
                                                 query')]
                            (when (and cascade-query? (not= query' query''))
                              (let [qs        (get @class-path->query classpath)
                                    q         (first qs)
                                    qnode     (zip/node
                                                (cond-> q
                                                        (nil? class) (treedb/query-template path)))
                                    new-query (assoc qnode
                                                prop query'')
                                    q'        (cond-> (zip/replace
                                                        (treedb/query-template (zip/root q) path)
                                                        new-query)
                                                      (nil? class)
                                                      (-> zip/root
                                                          (treedb/focus-query (pop path))
                                                          (treedb/query-template (pop path))))
                                    qs'       (into #{q'} (remove #{q}) qs)
                                    cp->q'    (merge {classpath qs'}
                                                     (cascade-query @class-path->query
                                                                    (pop classpath) path
                                                                    (zip/node q') union-keys))]
                                (swap! class-path->query merge cp->q')))
                            (build-index* class' query'' path' classpath query (conj union-keys prop)))))))))]
        (build-index* root-class rootq [] [] nil [])
        (swap! indexes merge
               {:prop->classes     @prop->classes
                :class-path->query @class-path->query}))))

  (index-component! [_ c]
    (swap! indexes
           (fn [indexes]
             (let [indexes (update-in ((:index-component extfs) indexes c)
                                      [:class->components (treedb/react-type c)]
                                      (fnil conj #{}) c)
                   data-path (into [] (remove number?) (com/path c))
                   indexes (update-in ((:index-component extfs) indexes c)
                                      [:data-path->components data-path]
                                      (fnil conj #{}) c)
                   ident     (when (satisfies? Ident c)
                               (let [ident (com/ident c (com/props c))]
                                 (invariant (util/ident? ident)
                                            (str "malformed Ident. An ident must be a vector of "
                                                 "two elements (a keyword and an EDN value). Check "
                                                 "the Ident implementation of component `"
                                                 (.. c -constructor -displayName) "`."))
                                 (invariant (some? (second ident))
                                            (str "component " (.. c -constructor -displayName)
                                                 "'s ident (" ident ") has a `nil` second element."
                                                 " This warning can be safely ignored if that is intended."))
                                 ident))]
               (if-not (nil? ident)
                 (cond-> indexes
                         ident (update-in [:ref->components ident] (fnil conj #{}) c))
                 indexes)))))

  (drop-component! [_ c]
    (swap! indexes
           (fn [indexes]
             (let [indexes (update-in ((:drop-component extfs) indexes c)
                                      [:class->components (treedb/react-type c)]
                                      disj c)
                   data-path (into [] (remove number?) (com/path c))
                   indexes (update-in ((:drop-component extfs) indexes c)
                                      [:data-path->components data-path]
                                      disj c)
                   ident     (when (satisfies? Ident c)
                               (com/ident c (com/props c)))]
               (if-not (nil? ident)
                 (cond-> indexes
                         ident (update-in [:ref->components ident] disj c))
                 indexes)))))

  (key->components [_ k]
    (let [indexes @indexes]
      (if (com/component? k)
        #{k}
        (if-let [cs ((:ref->components extfs) indexes k)]
          cs
          (transduce (map #(get-in indexes [:class->components %]))
                     (completing into)
                     (get-in indexes [:ref->components k] #{})
                     (get-in indexes [:prop->classes k])))))))

(defn indexer
  "Given a function (Component -> Ref), return an indexer."
  ([]
   (indexer
     {:index-component (fn [indexes component] indexes)
      :drop-component  (fn [indexes component] indexes)
      :ref->components (fn [indexes ref] nil)}))
  ([extfs]
   (Indexer.
     (atom
       {:class->components {}
        :data-path->components {}
        :ref->components   {}})
     extfs)))

(defn ref->any
  "Get any component from the indexer that matches the ref."
  [x ref]
  (let [indexer (if (db/reconciler? x) (db/get-indexer x) x)]
    (first (p/key->components indexer ref))))

(defn- merge-idents [tree config refs query]
  (let [{:keys [merge-ident indexer]} config
        ident-joins (into {} (filter #(and (util/join? %)
                                           (util/ident? (util/join-key %)))
                                     query))]
    (letfn [ (step [tree' [ident props]]
               (if (:normalize config)
                 (let [c-or-q (or (get ident-joins ident) (ref->any indexer ident))
                       props' (db/tree->db c-or-q props)
                       refs   (meta props')]
                   ((:merge-tree config)
                     (merge-ident config tree' ident props') refs))
                 (merge-ident config tree' ident props)))]
      (reduce step tree refs))))

(defn- sift-idents [res]
  (let [{idents true rest false} (group-by #(vector? (first %)) res)]
    [(into {} idents) (into {} rest)]))

(defn- merge-novelty!
  [reconciler state res query]
  (let [config      (:config reconciler)
        [idts res'] (sift-idents res)
        res'        (if (:normalize config)
                      (db/tree->db
                        (or query (:root @(:state reconciler)))
                        res' true)
                      res')]
    (-> state
        (merge-idents config idts query)
        ((:merge-tree config) res'))))

(defn default-merge [reconciler state res query]
  {:keys    (into [] (remove symbol?) (keys res))
   :next    (merge-novelty! reconciler state res query)
   :tempids (->> (filter (comp symbol? first) res)
                 (map (comp :tempids second))
                 (reduce merge {}))})

(defn merge!
  "Merge a state delta into the application state. Affected components managed
   by the reconciler will re-render."
  ([reconciler delta]
   (merge! reconciler delta nil))
  ([reconciler delta query]
   (merge! reconciler delta query nil))
  ([reconciler delta query remote]
   (let [config (:config reconciler)
         state (:state config)
         merge* (:merge config)
         {:keys [keys next tempids]} (merge* reconciler @state delta query)]
     (when (nil? remote)
       (p/queue! reconciler keys))
     (reset! state
             (if-let [migrate (:migrate config)]
               (merge (select-keys next [:om.next/queries])
                      (migrate next
                               (or query (db/get-query (:root @(:state reconciler))))
                               tempids (:id-key config)))
               next)))))

(defn default-merge-ident
  [_ tree ref props]
  (update-in tree ref merge props))

(defn default-merge-tree
  [a b]
  (if (map? a)
    (merge a b)
    b))

(defn default-migrate
  "Given app-state-pure (the application state as an immutable value), a query,
   tempids (a hash map from tempid to stable id), and an optional id-key
   keyword, return a new application state value with the tempids replaced by
   the stable ids."
  ([app-state-pure query tempids]
   (default-migrate app-state-pure query tempids nil))
  ([app-state-pure query tempids id-key]
   (letfn [(dissoc-in [pure [table id]]
             (assoc pure table (dissoc (get pure table) id)))
           (step [pure [old [_ id :as new]]]
             (-> pure
                 (dissoc-in old)
                 (assoc-in new
                           (cond-> (merge (get-in pure old) (get-in pure new))
                                   (not (nil? id-key)) (assoc id-key id)))))]
     (if-not (empty? tempids)
       (let [pure' (reduce step app-state-pure tempids)]
         (db/tree->db query
                   (db/db->tree query pure' pure'
                             (fn [ident] (get tempids ident ident))) true))
       app-state-pure))))

(defn- has-error?
  [x]
  (and (map? x) (contains? x ::error)))

(defn default-extract-errors [reconciler res query]
  (letfn [(extract* [query res errs]
            (let [class      (-> query meta :component)
                  top-error? (when (and (not (nil? class)) (has-error? res))
                               (swap! errs
                                      #(update-in % [((-> class meta :ident) class res)]
                                                  (fnil conj #{}) (::error res))))
                  ret        (when (nil? top-error?) {})]
              (cond
                ;; query root
                (vector? query)
                (if (vector? res)
                  (into [] (map #(extract* query % errs)) res)
                  (loop [exprs (seq query) ret ret]
                    (if-not (nil? exprs)
                      (let [expr (first exprs)
                            k    (as-> (treedb/expr->key expr) k
                                       (cond-> k
                                               (util/unique-ident? k) first))
                            data (get res k)]
                        (cond
                          (util/mutation? expr)
                          (let [mk   (util/mutation-key expr)
                                ret' (get res mk)]
                            (if (has-error? ret')
                              (let [x (-> expr meta :mutator)]
                                (swap! errs
                                       #(update-in % [x]
                                                   (fnil conj #{}) (::error ret')))
                                (recur (next exprs) ret))
                              (recur (next exprs)
                                     (when-not (nil? ret)
                                       (assoc ret mk ret')))))

                          (util/union? expr)
                          (let [jk     (util/join-key expr)
                                jv     (util/join-value expr)
                                class' (-> jv meta :component)]
                            (if (not (vector? data))
                              (let [ret' (extract*
                                           (get jv (first ((-> class' meta :ident) class' data)))
                                           data errs)]
                                (recur (next exprs)
                                       (when-not (nil? ret)
                                         (assoc ret jk ret'))))
                              (let [ret' (into []
                                               (map #(extract*
                                                       (get jv
                                                            (first ((-> class' meta :ident) class' %)))
                                                       % errs))
                                               data)]
                                (recur (next exprs)
                                       (when-not (nil? ret)
                                         (assoc ret jk ret'))))))

                          (util/join? expr)
                          (let [jk   (util/join-key expr)
                                jv   (util/join-value expr)
                                ret' (extract* jv data errs)]
                            (recur (next exprs)
                                   (when-not (nil? ret)
                                     (assoc ret jk ret'))))

                          (and (map? data) (has-error? data))
                          (do
                            (swap! errs
                                   #(update-in %
                                               [(or (when-not (nil? class)
                                                      ((-> class meta :ident) class res))
                                                    k)]
                                               (fnil conj #{}) (::error data)))
                            (recur (next exprs) nil))

                          :else
                          (recur (next exprs)
                                 (when-not (nil? ret)
                                   (assoc ret k data)))))
                      ret))))))]
    (let [errs (atom {})
          ret  (extract* query res errs)]
      {:tree ret :errors @errs})))

(defn- to-env [x]
  (let [config (if (treedb/reconciler? x) (:config x) x)]
    (select-keys config [:state :shared :parser :logger :pathopt])))

(defn- munge-component-name [x]
  (let [ns-name (-> x meta :component-ns)
        cl-name (-> x meta :component-name)]
    (munge
      (str (str/replace (str ns-name) "." "$") "$" cl-name))))

(defn- compute-react-key [cl props]
  (when-let [idx (-> props meta :om-path)]
    (str (munge-component-name cl) "_" idx)))

;; =============================================================================
;; Globals & Dynamics

(def ^{:dynamic true :private true} *instrument* nil)
(def ^{:dynamic true :private true} *reconciler* nil)
(def ^{:dynamic true :private true} *parent* nil)
(def ^{:dynamic true :private true} *shared* nil)
(def ^{:dynamic true :private true} *depth* 0)

;; =============================================================================

(defn factory
  "Create a factory constructor from a component class created with
   om.next/defui."
  ([class]
   (factory class nil))
  ([class {:keys [validator keyfn instrument?]
           :or {instrument? true} :as opts}]
   {:pre [(fn? class)]}
   (fn self
     ([] (self nil))
     ([props & children]
      (when-not (nil? validator)
        (assert (validator props)))
      (if (and *instrument* instrument?)
        (*instrument*
          {:props    props
           :children children
           :class    class
           :factory  (factory class (assoc opts :instrument? false))})
        (let [react-key (cond
                          (some? keyfn) (keyfn props)
                          (some? (:react-key props)) (:react-key props)
                          :else (compute-react-key class props))
              ctor class
              ref (:ref props)
              props {:omcljs$reactRef   ref
                     :omcljs$reactKey   react-key
                     :omcljs$value      (cond-> props
                                                (map? props) (dissoc :ref))
                     :omcljs$mounted?   (atom false)
                     :omcljs$path       (-> props meta :om-path)
                     :omcljs$reconciler *reconciler*
                     :omcljs$parent     *parent*
                     :omcljs$shared     *shared*
                     :omcljs$instrument *instrument*
                     :omcljs$depth      *depth*}
              component (ctor (atom nil) (atom nil) props children)]
          (when ref
            (assert (some? *parent*))
            (swap! (:refs *parent*) assoc ref component))
          (reset! (:state component) (.initLocalState component))
          component))))))

(defn gather-sends
  "Given an environment, a query and a set of remotes return a hash map of remotes
   mapped to the query specific to that remote."
  [{:keys [parser] :as env} q remotes]
  (into {}
        (comp
          (map #(vector % (parser env q %)))
          (filter (fn [[_ v]] (pos? (count v)))))
        remotes))

(defrecord Reconciler [config state]
  clojure.lang.IDeref
  (deref [this] @(:state config))

  p/IReconciler
  (basis-t [_] (:t @state))

  (add-root! [this root-class target options]
    (let [ret   (atom nil)
          rctor (factory root-class)
          guid  (java.util.UUID/randomUUID)]
      (when (com/iquery? root-class)
        (p/index-root (:indexer config) root-class))
      (when (and (:normalize config)
                 (not (:normalized @state)))
        (let [new-state (db/tree->db root-class @(:state config))
              refs      (meta new-state)]
          (reset! (:state config) (merge new-state refs))
          (swap! state assoc :normalized true)))
      (let [renderf (fn [data]
                      (binding [*reconciler* this
                                *shared*     (merge
                                               (:shared config)
                                               (when (:shared-fn config)
                                                 ((:shared-fn config) data)))
                                *instrument* (:instrument config)]
                        (let [c (cond
                                  (nil? @ret) (rctor data)
                                  :else (when-let [c' @ret]
                                          c'))]
                          (when (and (nil? @ret) (not (nil? c)))
                            (swap! state assoc :root c)
                            (reset! ret c)))))
            parsef  (fn []
                      (let [sel (db/get-query (or @ret root-class))]
                        (assert (or (nil? sel) (vector? sel))
                                "Application root query must be a vector")
                        (if-not (nil? sel)
                          (let [env (to-env config)
                                v   ((:parser config) env sel)]
                            (when-not (empty? v)
                              (renderf v)))
                          (renderf @(:state config)))))]
        (swap! state merge
               {:target target :render parsef :root root-class
                :remove (fn []
                          (remove-watch (:state config) (or target guid))
                          (swap! state
                                 #(-> %
                                      (dissoc :target) (dissoc :render) (dissoc :root)
                                      (dissoc :remove)))
                          (when-not (nil? target)
                            ((:root-unmount config) target)))})
        (add-watch (:state config) (or target guid)
                   (fn [_ _ _ _]
                     (swap! state update-in [:t] inc)))
        (parsef)
        (when-let [sel (db/get-query (or (and target @ret) root-class))]
          (let [env  (to-env config)
                snds (gather-sends env sel (:remotes config))]
            (when-not (empty? snds)
              (when-let [send (:send config)]
                (send snds
                      (fn send-cb
                        ([resp]
                         (merge! this resp nil)
                         (renderf ((:parser config) env sel)))
                        ([resp query]
                         (merge! this resp query)
                         (renderf ((:parser config) env sel)))
                        ([resp query remote]
                         (when-not (nil? remote)
                           (p/queue! this (keys resp) remote))
                         (merge! this resp query remote)
                         (p/reconcile! this remote))))))))
        @ret)))

  (remove-root! [_ target]
    (when-let [remove (:remove @state)]
      (remove)))

  (reindex! [this]
    (let [root (get @state :root)]
      (when (com/iquery? root)
        (let [indexer (:indexer config)
              c (first (get-in @indexer [:class->components root]))]
          (p/index-root indexer (or c root))))))

  (queue! [this ks]
    (p/queue! this ks nil))
  (queue! [_ ks remote]
    (if-not (nil? remote)
      (swap! state update-in [:remote-queue remote] into ks)
      (swap! state update-in [:queue] into ks)))

  (queue-sends! [_ sends]
    (swap! state update-in [:queued-sends]
           (:merge-sends config) sends))

  (schedule-render! [_]
    (if-not (:queued @state)
      (do
        (swap! state assoc :queued true)
        true)
      false))

  (schedule-sends! [_]
    (if-not (:sends-queued @state)
      (do
        (swap! state assoc :sends-queued true)
        true)
      false))

  (reconcile! [this]
    (p/reconcile! this nil))
  ;; TODO: need to reindex roots after reconcilation
  (reconcile! [this remote]
    (let [st @state
          q (if-not (nil? remote)
              (get-in st [:remote-queue remote])
              (:queue st))]
      (swap! state update-in [:queued] not)
      (if (not (nil? remote))
        (swap! state assoc-in [:remote-queue remote] [])
        (swap! state assoc :queue []))
      (if (empty? q)
        ;; TODO: need to move root re-render logic outside of batching logic
        ((:render st))
        (let [cs (transduce
                   (map #(p/key->components (:indexer config) %))
                   #(into %1 %2) #{} q)
              {:keys [ui->props]} config
              env (to-env config)
              root (:root @state)]))))

  (send! [this]
    (let [sends (:queued-sends @state)]
      (when-not (empty? sends)
        (swap! state
               (fn [state]
                 (-> state
                     (assoc :queued-sends {})
                     (assoc :sends-queued false))))
        ((:send config) sends
          (fn send-cb
            ([resp]
             (merge! this resp nil))
            ([resp query]
             (merge! this resp query))
            ([resp query remote]
             (when-not (nil? remote)
               (p/queue! this (keys resp) remote))
             (merge! this resp query remote)
             (p/reconcile! this remote))))))))



