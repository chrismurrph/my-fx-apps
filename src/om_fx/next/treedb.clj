(ns om-fx.next.treedb
  (:require [om-fx.util :as util]
            [om-fx.next.common :as com]
            [clojure.zip :as zip]
            [om-fx.next.protocols :as p]
            [clojure.reflect :as reflect]
            [clojure.string :as str])
  (:import (clojure.lang IObj IMapEntry)))

(defn- normalize* [query data refs union-seen]
  (cond
    (= '[*] query) data

    ;; union case
    (map? query)
    (let [class (-> query meta :component)
          ident   (when-let [ident (-> class meta :ident)]
                    (ident class data))]
      (if-not (nil? ident)
        (vary-meta (normalize* (get query (first ident)) data refs union-seen)
                   assoc :om/tag (first ident))
        (throw (IllegalArgumentException. "Union components must implement Ident"))))

    (vector? data) data ;; already normalized

    :else
    (loop [q (seq query) ret data]
      (if-not (nil? q)
        (let [expr (first q)]
          (if (util/join? expr)
            (let [[k sel] (util/join-entry expr)
                  recursive? (util/recursion? sel)
                  union-entry (if (util/union? expr) sel union-seen)
                  sel     (if recursive?
                            (if-not (nil? union-seen)
                              union-seen
                              query)
                            sel)
                  class   (-> sel meta :component)
                  v       (get data k)]
              (cond
                ;; graph loop: db->tree leaves ident in place
                (and recursive? (util/ident? v)) (recur (next q) ret)
                ;; normalize one
                (map? v)
                (let [x (normalize* sel v refs union-entry)]
                  (if-not (or (nil? class) (not (-> class meta :ident)))
                    (let [i ((-> class meta :ident) class v)]
                      (swap! refs update-in [(first i) (second i)] merge x)
                      (recur (next q) (assoc ret k i)))
                    (recur (next q) (assoc ret k x))))

                ;; normalize many
                (vector? v)
                (let [xs (into [] (map #(normalize* sel % refs union-entry)) v)]
                  (if-not (or (nil? class) (not (-> class meta :ident)))
                    (let [is (into [] (map #((-> class meta :ident) class %)) xs)]
                      (if (vector? sel)
                        (when-not (empty? is)
                          (swap! refs
                                 (fn [refs]
                                   (reduce (fn [m [i x]]
                                             (update-in m i merge x))
                                           refs (zipmap is xs)))))
                        ;; union case
                        (swap! refs
                               (fn [refs']
                                 (reduce
                                   (fn [ret [i x]]
                                     (update-in ret i merge x))
                                   refs' (map vector is xs)))))
                      (recur (next q) (assoc ret k is)))
                    (recur (next q) (assoc ret k xs))))

                ;; missing key
                (nil? v)
                (recur (next q) ret)

                ;; can't handle
                :else (recur (next q) (assoc ret k v))))
            (let [k (if (seq? expr) (first expr) expr)
                  v (get data k)]
              (if (nil? v)
                (recur (next q) ret)
                (recur (next q) (assoc ret k v))))))
        ret))))

(defprotocol IQueryParams
  (params [this] "Return the query parameters"))

(defn get-reconciler
  [c]
  {:pre [(com/component? c)]}
  (com/get-prop c :omcljs$reconciler))

(defn- component->query-data [component]
  (some-> (get-reconciler component)
          :config :state deref ::queries (get component)))

(defn- var->keyword [x]
  (keyword (.substring (str x) 1)))

(defn- replace-var [expr params]
  (if (var? expr)
    (get params (var->keyword expr) expr)
    expr))

(defn- bind-query [query params]
  (let [qm (meta query)
        tr (map #(bind-query % params))
        ret (cond
              (seq? query) (apply list (into [] tr query))
              (instance? IMapEntry query) (into [] tr query)
              (coll? query) (into (empty query) tr query)
              :else (replace-var query params))]
    (cond-> ret
            (and qm (instance? IObj ret))
            (with-meta qm))))

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

(defn get-component-query
  ([component]
   (get-component-query component (component->query-data component)))
  ([component query-data]
   (let [q  (:query query-data (com/query component))
         c' (-> q meta :component)]
     (assert (nil? c')
             (str "Query violation, " component " reuses " c' " query"))
     (with-meta
       (bind-query q (:params query-data (params component)))
       {:component (react-type component)}))))

(defn- get-class-or-instance-query
  "Return a IQuery/IParams local bound query. Works for component classes
   and component instances. Does not use the indexer."
  [x]
  (if (com/component? x)
    (get-component-query x)
    (let [q ((-> x meta :query) x)
          c (-> q meta :component)]
      (assert (nil? c) (str "Query violation, " x , " reuses " c " query"))
      (with-meta (bind-query q (params x)) {:component x}))))

;; this function assumes focus is actually in fact
;; already focused!
(defn focus->path
  "Given a focused query return the path represented by the query.

   Examples:

     (om.next/focus->path [{:foo [{:bar {:baz []}]}])
     => [:foo :bar :baz]"
  ([focus]
   (focus->path focus '* []))
  ([focus bound]
   (focus->path focus bound []))
  ([focus bound path]
   (if (and (or (= bound '*)
                (and (not= path bound)
                     (< (count path) (count bound))))
            (some util/join? focus)
            (== 1 (count focus)))
     (let [[k focus'] (util/join-entry (first focus))
           focus'     (if (util/recursion? focus')
                        focus
                        focus')]
       (recur focus' bound (conj path k)))
     path)))

(defn- get-indexed-query
  "Get a component's static query from the indexer. For recursive queries, recurses
   up the data path. Falls back to `get-class-or-instance-query` if nothing is
   found in the indexer."
  [component class-path-query-data data-path]
  (let [qs (filter #(= data-path (-> % zip/root (focus->path data-path)))
                   class-path-query-data)
        qs (if (empty? qs) class-path-query-data qs)]
    (if-not (empty? qs)
      (let [q (first qs)
            node (zip/node q)]
        (if-not (util/recursion? node)
          node
          (recur component class-path-query-data (pop data-path))))
      (get-class-or-instance-query component))))

(defn reconciler?
  "Returns true if x is a reconciler."
  [x]
  (or (instance? p/IReconciler x)
      (satisfies? p/IReconciler x)))

(defn get-indexer
  "PRIVATE: Get the indexer associated with the reconciler."
  [reconciler]
  {:pre [(reconciler? reconciler)]}
  (-> reconciler :config :indexer))

(defn- parent
  "Returns the parent Om component."
  [component]
  (com/get-prop component :omcljs$parent))

(defn raw-class-path
  "Return the raw component class path associated with a component. Contains
   duplicates for recursive component trees."
  [c]
  (loop [c c ret (list (react-type c))]
    (if-let [p (parent c)]
      (if (com/iquery? p)
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

(defn get-query
  "Return a IQuery/IParams instance bound query. Works for component classes
   and component instances. See also om.next/full-query."
  [x]
  (when (com/iquery? x)
    (if (com/component? x)
      (if-let [query-data (component->query-data x)]
        (get-component-query x query-data)
        (let [cp (class-path x)
              r (get-reconciler x)
              data-path (into [] (remove number?) (com/path x))
              class-path-query-data (get (:class-path->query @(get-indexer r)) cp)]
          (get-indexed-query x class-path-query-data data-path)))
      (get-class-or-instance-query x))))

(defn tree->db
  "Given a Om component class or instance and a tree of data, use the component's
   query to transform the tree into the default database format. All nodes that
   can be mapped via Ident implementations wil be replaced with ident links. The
   original node data will be moved into tables indexed by ident. If merge-idents
   option is true, will return these tables in the result instead of as metadata."
  ([x data]
   (tree->db x data false))
  ([x data merge-idents ]
   (let [refs (atom {})
         x    (if (vector? x) x (get-query x))
         ret  (normalize* x data refs nil)]
     (if merge-idents
       (let [refs' @refs]
         (assoc (merge ret refs')
           ::tables (into #{} (keys refs'))))
       (with-meta ret @refs)))))

(defn- mappable-ident? [refs ident]
  (and (util/ident? ident)
       (contains? refs (first ident))))

(declare focus-query*)

(defn- focused-join [expr ks full-expr union-expr]
  (let [expr-meta (meta expr)
        expr' (cond
                (map? expr)
                (let [join-value (-> expr first second)
                      join-value (if (and (util/recursion? join-value)
                                          (seq ks))
                                   (if-not (nil? union-expr)
                                     union-expr
                                     full-expr)
                                   join-value)]
                  {(ffirst expr) (focus-query* join-value ks nil)})

                (seq? expr) (list (focused-join (first expr) ks nil nil) (second expr))
                :else       expr)]
    (cond-> expr'
            (some? expr-meta) (with-meta expr-meta))))

(defn- focus-query*
  [query path union-expr]
  (if (empty? path)
    query
    (let [[k & ks] path]
      (letfn [(match [x]
                (= k (util/join-key x)))
              (value [x]
                (focused-join x ks query union-expr))]
        (if (map? query) ;; UNION
          {k (focus-query* (get query k) ks query)}
          (into [] (comp (filter match) (map value) (take 1)) query))))))

(defn focus-query
  "Given a query, focus it along the specified path.

  Examples:
    (om.next/focus-query [:foo :bar :baz] [:foo])
    => [:foo]

    (om.next/focus-query [{:foo [:bar :baz]} :woz] [:foo :bar])
    => [{:foo [:bar]}]"
  [query path]
  (focus-query* query path nil))

(defn expr->key
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

(defn- move-to-key
  "Move from the current zipper location to the specified key. loc must be a
   hash map node."
  [loc k]
  (loop [loc (zip/down loc)]
    (let [node (zip/node loc)]
      (if (= k (first node))
        (-> loc zip/down zip/right)
        (recur (zip/right loc))))))

(defn- query-zip
  "Return a zipper on a query expression."
  [root]
  (zip/zipper
    #(or (vector? %) (map? %) (seq? %))
    seq
    (fn [node children]
      (let [ret (cond
                  (vector? node) (vec children)
                  (map? node)    (into {} children)
                  (seq? node)    children)]
        (with-meta ret (meta node))))
    root))

(defn query-template
  "Given a query and a path into a query return a zipper focused at the location
   specified by the path. This location can be replaced to customize / alter
   the query."
  [query path]
  (letfn [(query-template* [loc path]
            (if (empty? path)
              loc
              (let [node (zip/node loc)]
                (if (vector? node) ;; SUBQUERY
                  (recur (zip/down loc) path)
                  (let [[k & ks] path
                        k' (expr->key node)]
                    (if (= k k')
                      (if (or (map? node)
                              (and (seq? node) (map? (first node))))
                        (let [loc'  (move-to-key (cond-> loc (seq? node) zip/down) k)
                              node' (zip/node loc')]
                          (if (map? node') ;; UNION
                            (if (seq ks)
                              (recur
                                (zip/replace loc'
                                             (zip/node (move-to-key loc' (first ks))))
                                (next ks))
                              loc')
                            (recur loc' ks))) ;; JOIN
                        (recur (-> loc zip/down zip/down zip/down zip/right) ks)) ;; CALL
                      (recur (zip/right loc) path)))))))]
    (query-template* (query-zip query) path)))

(defn reduce-query-depth
  "Changes a join on key k with depth limit from [:a {:k n}] to [:a {:k (dec n)}]"
  [q k]
  (if-not (empty? (focus-query q [k]))
    (let [pos (query-template q [k])
          node (zip/node pos)
          node' (cond-> node (number? node) dec)]
      (replace pos node'))
    q))

(defn- reduce-union-recursion-depth
  "Given a union expression decrement each of the query roots by one if it
   is recursive."
  [union-expr recursion-key]
  (->> union-expr
       (map (fn [[k q]] [k (reduce-query-depth q recursion-key)]))
       (into {})))


(defn- denormalize*
  "Denormalize a data based on query. refs is a data structure which maps idents
   to their values. map-ident is a function taking a ident to another ident,
   used during tempid transition. idents-seen is the set of idents encountered,
   used to limit recursion. union-expr is the current union expression being
   evaluated. recurse-key is key representing the current recursive query being
   evaluted."
  [query data refs map-ident idents-seen union-expr recurse-key]
  ;; support taking ident for data param
  (let [union-recur? (and union-expr recurse-key)
        recur-ident (when union-recur?
                      data)
        data (loop [data data]
               (if (mappable-ident? refs data)
                 (recur (get-in refs (map-ident data)))
                 data))]
    (cond
      (vector? data)
      ;; join
      (let [step (fn [ident]
                   (if-not (mappable-ident? refs ident)
                     (if (= query '[*])
                       ident
                       (let [{props false joins true} (group-by util/join? query)
                             props (mapv #(cond-> % (seq? %) first) props)]
                         (loop [joins (seq joins) ret {}]
                           (if-not (nil? joins)
                             (let [join        (first joins)
                                   [key sel]   (util/join-entry join)
                                   v           (get ident key)]
                               (recur (next joins)
                                      (assoc ret
                                        key (denormalize* sel v refs map-ident
                                                          idents-seen union-expr recurse-key))))
                             (merge (select-keys ident props) ret)))))
                     (let [ident'       (get-in refs (map-ident ident))
                           query        (cond-> query
                                                union-recur? (reduce-union-recursion-depth recurse-key))
                           ;; also reduce query depth of union-seen, there can
                           ;; be more union recursions inside
                           union-seen'  (cond-> union-expr
                                                union-recur? (reduce-union-recursion-depth recurse-key))
                           query'       (cond-> query
                                                (map? query) (get (first ident)))] ;; UNION
                       (denormalize* query' ident' refs map-ident idents-seen union-seen' nil))))]
        (into [] (map step) data))

      (and (map? query) union-recur?)
      (denormalize* (get query (first recur-ident)) data refs map-ident
                    idents-seen union-expr recurse-key)

      :else
      ;; map case
      (if (= '[*] query)
        data
        (let [{props false joins true} (group-by #(or (util/join? %)
                                                      (util/ident? %)
                                                      (and (seq? %)
                                                           (util/ident? (first %))))
                                                 query)
              props (mapv #(cond-> % (seq? %) first) props)]
          (loop [joins (seq joins) ret {}]
            (if-not (nil? joins)
              (let [join        (first joins)
                    join        (cond-> join
                                        (seq? join) first)
                    join        (cond-> join
                                        (util/ident? join) (hash-map '[*]))
                    [key sel]   (util/join-entry join)
                    recurse?    (util/recursion? sel)
                    recurse-key (when recurse? key)
                    v           (if (util/ident? key)
                                  (if (= '_ (second key))
                                    (get refs (first key))
                                    (get-in refs (map-ident key)))
                                  (get data key))
                    key         (cond-> key (util/unique-ident? key) first)
                    v           (if (mappable-ident? refs v)
                                  (loop [v v]
                                    (let [next (get-in refs (map-ident v))]
                                      (if (mappable-ident? refs next)
                                        (recur next)
                                        (map-ident v))))
                                  v)
                    limit       (if (number? sel) sel :none)
                    union-entry (if (util/union? join)
                                  sel
                                  (when recurse?
                                    union-expr))
                    sel         (cond
                                  recurse?
                                  (if-not (nil? union-expr)
                                    union-entry
                                    (reduce-query-depth query key))

                                  (and (mappable-ident? refs v)
                                       (util/union? join))
                                  (get sel (first v))

                                  (and (util/ident? key)
                                       (util/union? join))
                                  (get sel (first key))

                                  :else sel)
                    graph-loop? (and recurse?
                                     (contains? (set (get idents-seen key)) v)
                                     (= :none limit))
                    idents-seen (if (and (mappable-ident? refs v) recurse?)
                                  (-> idents-seen
                                      (update-in [key] (fnil conj #{}) v)
                                      (assoc-in [:last-ident key] v)) idents-seen)]
                (cond
                  (= 0 limit) (recur (next joins) ret)
                  graph-loop? (recur (next joins) ret)
                  (nil? v)    (recur (next joins) ret)
                  :else       (recur (next joins)
                                     (assoc ret
                                       key (denormalize* sel v refs map-ident
                                                         idents-seen union-entry recurse-key)))))
              (if-let [looped-key (some
                                    (fn [[k identset]]
                                      (if (contains? identset (get data k))
                                        (get-in idents-seen [:last-ident k])
                                        nil))
                                    (dissoc idents-seen :last-ident))]
                looped-key
                (merge (select-keys data props) ret)))))))))

(defn db->tree
  "Given a query, some data in the default database format, and the entire
   application state in the default database format, return the tree where all
   ident links have been replaced with their original node values."
  ([query data refs]
   {:pre [(map? refs)]}
   (denormalize* query data refs identity {} nil nil))
  ([query data refs map-ident]
   {:pre [(map? refs)]}
   (denormalize* query data refs map-ident {} nil nil)))

