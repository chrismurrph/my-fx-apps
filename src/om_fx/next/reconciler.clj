(ns om-fx.next.reconciler
  (:require [om-fx.next.protocols :as p]
            [om-fx.util :as util]
            [om-fx.next.common :as com]
            [om-fx.next.treedb :as db]
            [clojure.zip :as zip]
            [clojure.string :as str]
            [clojure.reflect :as reflect]
            [om-fx.next.impl.parser :as parser])
  (:import (om_fx.next.common Ident)
           (clojure.lang IDeref)
           (om_fx.next.treedb IQuery)))

(defn iquery?
  [x]
  (if (fn? x)
    (some? (-> x meta :query))
    (let [class (cond-> x (com/component? x) class)]
      (extends? IQuery class))))

(defn react-type
  "Returns the component type, regardless of whether the component has been
   mounted"
  [component]
  {:pre [(com/component? component)]}
  (let [[klass-name] (str/split (reflect/typename (type component)) #"_klass")
        last-idx-dot (.lastIndexOf klass-name ".")
        ns (clojure.main/demunge (subs klass-name 0 last-idx-dot))
        c (subs klass-name (inc last-idx-dot))]
    @(or (find-var (symbol ns c))
         (find-var (symbol ns (clojure.main/demunge c))))))

(defn- parent
  "Returns the parent Om component."
  [component]
  (com/get-prop component :omcljs$parent))

(defn- raw-class-path
  "Return the raw component class path associated with a component. Contains
   duplicates for recursive component trees."
  [c]
  (loop [c c ret (list (react-type c))]
    (if-let [p (parent c)]
      (if (iquery? p)
        (recur p (cons (react-type p) ret))
        (recur p ret))
      ret)))

(defn class-path
  "Return the component class path associated with a component."
  [c]
  {:pre [(com/component? c)]}
  (let [raw-cp (raw-class-path c)]
    (loop [cp (seq raw-cp) ret [] seen #{}]
      (if cp
        (let [c (first cp)]
          (if (contains? seen c)
            (recur (next cp) ret seen)
            (recur (next cp) (conj ret c) (conj seen c))))
        (seq ret)))))

(defn- recursive-class-path?
  "Returns true if a component's classpath is recursive"
  #?(:cljs {:tag boolean})
  [c]
  {:pre [(com/component? c)]}
  (not (apply distinct? (raw-class-path c))))

(defn- get-dispatch-key [prop]
  (cond-> prop
          (or (not (util/ident? prop))
              (= (second prop) '_))
          ((comp :dispatch-key parser/expr->ast))))

(defn full-query
  "Returns the absolute query for a given component, not relative like
   om.next/get-query."
  ([component]
   (when (iquery? component)
     (if (nil? (com/path component))
       (replace
         (first
           (get-in @(-> component db/get-reconciler db/get-indexer)
                   [:class-path->query (class-path component)]))
         (db/get-query component))
       (full-query component (db/get-query component)))))
  ([component query]
   (when (iquery? component)
     (let [xf    (cond->> (remove number?)
                          (recursive-class-path? component) (comp (distinct)))
           path' (into [] xf (com/path component))
           cp    (class-path component)
           qs    (get-in @(-> component db/get-reconciler db/get-indexer)
                         [:class-path->query cp])]
       (if-not (empty? qs)
         ;; handle case where child appears multiple times at same class-path
         ;; but with different queries
         (let [q (->> qs
                      (filter #(= path'
                                  (mapv get-dispatch-key
                                        (-> % zip/root (focus->path path')))))
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
  (let [ui (when (and pathopt (satisfies? Ident c) (iquery? c))
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

(defn react-type
  "Returns the component type, regardless of whether the component has been
   mounted"
  [component]
  {:pre [(com/component? component)]}
  (let [[klass-name] (str/split (reflect/typename (type component)) #"_klass")
        last-idx-dot (.lastIndexOf klass-name ".")
        ns (clojure.main/demunge (subs klass-name 0 last-idx-dot))
        c (subs klass-name (inc last-idx-dot))]
    @(or (find-var (symbol ns c))
         (find-var (symbol ns (clojure.main/demunge c))))))

(defrecord Indexer [indexes extfs]
  IDeref
  (deref [_] @indexes)

  p/IIndexer
  (index-root [_ x]
    (let [prop->classes     (atom {})
          class-path->query (atom {})
          rootq             (db/get-query x)
          root-class        (cond-> x (com/component? x) react-type)]
      (letfn [(build-index* [class query path classpath union-expr union-keys]
                (invariant (or (not (iquery? class))
                               (and (iquery? class)
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
                             (query-template (focus-query root-query path) path))))
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
                                                         (react-type (first cs)))
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
                                cs             (filter #(= class' (react-type %))
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
                                                        (nil? class) (query-template path)))
                                    new-query (assoc qnode
                                                prop query'')
                                    q'        (cond-> (zip/replace
                                                        (query-template (zip/root q) path)
                                                        new-query)
                                                      (nil? class)
                                                      (-> zip/root
                                                          (focus-query (pop path))
                                                          (query-template (pop path))))
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
                                      [:class->components (react-type c)]
                                      (fnil conj #{}) c)
                   data-path (into [] (remove number?) (com/path c))
                   indexes (update-in ((:index-component extfs) indexes c)
                                      [:data-path->components data-path]
                                      (fnil conj #{}) c)
                   ident     (when #?(:clj  (satisfies? Ident c)
                                      :cljs (implements? Ident c))
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
                                      [:class->components (react-type c)]
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

(defn- expr->key
  "Given a query expression return its key."
  [expr]
  (cond
    (keyword? expr) expr
    (map? expr)     (ffirst expr)
    (seq? expr)     (let [expr' (first expr)]
                      (when (map? expr')
                        (ffirst expr')))
    (util/ident? expr)   (cond-> expr (= '_ (second expr)) first)
    :else
    (throw
      (ex-info (str "Invalid query expr " expr)
               {:type :error/invalid-expression}))))

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
                            k    (as-> (expr->key expr) k
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



