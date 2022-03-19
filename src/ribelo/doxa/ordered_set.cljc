(ns ribelo.doxa.ordered-set
  (:require
   [ribelo.extropy :as ex]))

#?(:clj (set! *warn-on-reflection* true))

(declare transient-ordered-set)

(deftype -skiping-iterator [#?(:clj ^java.util.Iterator it :cljs ^js it) ^:unsynchronized-mutable x]
  #?(:clj java.util.Iterator :cljs Object)
  (hasNext [_]
    (if-not (identical? x ex/-sentinel)
      true
      (loop []
        (if (.hasNext it)
          (let [v (.next it)]
            (if-not (identical? v ex/-sentinel)
              (do (set! x v) true)
              (recur)))
          false))))
  (next [this]
    (if (.hasNext this)
      (let [v x] (set! x ex/-sentinel) v)
      (throw (ex-info "No such element" {})))))

#?(:clj
   (deftype OrderedSet [meta ^clojure.lang.IPersistentMap m ^clojure.lang.IPersistentVector xs]
     java.util.Set

     Iterable
     (iterator [_]
       (-skiping-iterator. (ex/-iter xs) ex/-sentinel))

     clojure.lang.IObj
     (withMeta [this new-meta]
       (if (identical? new-meta meta)
         this
         (OrderedSet. new-meta m xs)))

     clojure.lang.Seqable
     (seq [_] (.seq ^clojure.lang.IPersistentVector (ex/-remove (partial identical? ex/-sentinel) xs)))

     clojure.lang.IMeta
     (meta [_] meta)

     java.util.Collection
     (size [_] (.count m))

     clojure.lang.Counted
     (count [_] (.count m))

     clojure.lang.IReduce
     (reduce [_ f]
       (reduce f (ex/-remove (partial identical? ex/-sentinel) xs)))

     clojure.lang.IReduceInit
     (reduce [_ f v]
       (reduce f v (ex/-remove (partial identical? ex/-sentinel) xs)))

     clojure.lang.IPersistentSet
     (contains [_ k]
       (boolean (.valAt m k)))
     (disjoin [this v]
       (if-let [i (.valAt m v)]
         (OrderedSet. meta (dissoc m v) (.assoc xs i ex/-sentinel))
         this))

     clojure.lang.IPersistentCollection
     (equiv [this o]
       (and
         (instance? OrderedSet o)
         (== (count this) (count o))
         (loop [xs (ex/-iter this) ys (ex/-iter o)]
           (if (.hasNext xs)
             (if (= (.next xs) (.next ys))
               (recur xs ys)
               false)
             true))))

     clojure.lang.IEditableCollection
     (asTransient [this]
       (transient-ordered-set this))

     (cons [this v]
       (if (.containsKey m v)
         this
         (OrderedSet. meta (.assoc ^clojure.lang.Associative m v (.count xs)) (.cons xs v))))

     (empty [_]
       (OrderedSet. {} {} []))

     clojure.lang.ILookup
     (valAt [_ v]
       (if (.containsKey m v)
         v
         nil))
     (valAt [_ v not-found]
       (if (.containsKey m v)
         v
         not-found))

     clojure.lang.IFn
     (invoke [this k]
       (.valAt this k))
     (invoke [this k not-found]
       (.valAt this k not-found)))

   :cljs
   (deftype OrderedSet [meta m xs]
     Object
     (toString [coll]
       (pr-str* coll))
     (equiv [this other]
       (-equiv this other))
     (has [coll k]
       (contains? coll k))

     IIterable
     (-iterator [_]
       (-skiping-iterator. (ex/-iter xs) ex/-sentinel))

     IPrintWithWriter
     (-pr-writer [_ writer opts]
       (-write writer "#ordered/set ")
       (-write writer "#{")
       (let [it (ex/-iter xs)]
         (loop [begin? true]
           (when (.hasNext it)
             (let [x (.next it)]
               (when-not (identical? x ex/-sentinel)
                 (when-not begin?
                   (-write writer \space))
                 (-write writer (pr-str x))))
             (recur false))))
       (-write writer "}"))

     IWithMeta
     (-with-meta [this new-meta]
       (if (identical? new-meta meta)
         this
         (OrderedSet. new-meta m xs)))

     IMeta
     (-meta [_] meta)

     ISeqable
     (-seq [_] (-seq (ex/-remove (partial identical? ex/-sentinel) xs)))

     IEquiv
     (-equiv [this ^js o]
       (and
         (instance? OrderedSet o)
         (== (-count m) (-count (.-m o)))
         ^boolean
         (ex/-loop [x this y o :let [acc true]]
           (if (= x y)
             (recur true)
             false)
           acc)))

     ISet
     (-disjoin [this v]
       (if-let [i (.get m v)]
         (OrderedSet.
           meta
           (-dissoc m v)
           (-assoc xs i ex/-sentinel))
         this))

     ICollection
     (-conj [this v]
       (if (.get m v)
         this
         (OrderedSet.
           meta
           (-assoc m v (-count xs))
           (-conj xs v))))

     IEmptyableCollection
     (-empty [_]
       (OrderedSet. {} {} []))

     IEditableCollection
     (-as-transient [this]
       (transient-ordered-set this))

     ICounted
     (-count [_] (-count m))

     ILookup
     (-lookup [_ v]
       (if (.get m v)
         v
         nil))

     (-lookup [_ v not-found]
       (if (.get m v)
         v
         not-found))))

#?(:clj
   (defn ordered-set
     ([] (ordered-set []))
     ([xs]
      (reduce
        (fn [acc x] (conj acc x))
        (OrderedSet. {} {} [])
        xs)))
   :cljs
   (defn ordered-set
     ([] (ordered-set []))
     ([xs]
      (reduce
        (fn [acc x]
          (conj acc x))
        (OrderedSet. {} {} [])
        xs))))

(defn ordered-set? [xs]
  #?(:clj  (instance? OrderedSet xs)
     :cljs (instance? OrderedSet xs)))

#?(:clj
   (deftype TransientOrderedSet [meta ^clojure.lang.ITransientMap ^:unsynchronized-mutable m
                                 ^clojure.lang.ITransientVector ^:unsynchronized-mutable xs]
     clojure.lang.ITransientSet
     (count [_]
       (.count xs))
     (get [_ k]
       (when (.valAt m k) k))
     (disjoin [this k]
       (let [i (.valAt m k)]
         (when i
           (set! m (.without m k))
           (set! xs (.assocN xs i ex/-sentinel))))
       this)
     (conj [this k]
       (let [i (.valAt m k)]
         (when-not i
           (set! m (.assoc ^clojure.lang.ITransientAssociative m k (.count xs)))
           (set! xs (.conj ^clojure.lang.ITransientCollection xs k))))
       this)
     (contains [_ k]
       (boolean (.valAt m k)))
     (persistent [_]
       (OrderedSet. meta (.persistent m) (.persistent xs))))

   :cljs
   (deftype TransientOrderedSet [meta m xs]
     ITransientSet
     (-disjoin! [this v]
       (if-let [i (m v)]
         (TransientOrderedSet.
           meta
           (-dissoc! m v)
           (-assoc! xs i ex/-sentinel))
         this))

     ICounted
     (-count [_] (-count xs))

     ILookup
     (-lookup [_ v]
       (if (m v)
         v
         nil))

     ITransientCollection
     (-conj! [this v]
       (if (m v)
         this
         (TransientOrderedSet.
           meta
           (-assoc! m v (-count xs))
           (-conj! xs v))))
     (-persistent! [_]
       (OrderedSet. meta (persistent! m) (persistent! xs)))))

(defn transient-ordered-set [^OrderedSet ordered-set]
  (TransientOrderedSet. (.-meta ordered-set) (transient (.-m ordered-set)) (transient (.-xs ordered-set))))

#?(:clj
   (defmethod print-method OrderedSet
     [x ^java.io.Writer writer]
     (.write writer "#ordered/set ")
     (.write writer "#{")
     (let [it (ex/-iter x)]
       (loop [begin? true]
           (when (.hasNext it)
             (let [x (.next it)]
               (when-not (identical? x ex/-sentinel)
                 (when-not begin?
                   (.write writer " "))
                 (.write writer (pr-str x))))
             (recur false))))
     (.write writer "}")))

#?(:cljs
   (def transit-write-handlers
     {OrderedSet
      (reify Object
        (tag [_ _] "dx/os")
        (rep [_ ^js os] (.-xs os))
        (stringRep [_ _] nil)
        (verboseHandler [_] nil))}))

(def transit-read-handlers
  {"dx/os" (fn [x] (ordered-set x))})
