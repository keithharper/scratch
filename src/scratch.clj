(ns scratch)

(defn pull-keys
  "Returns a map containing only those entries in map whose key is in pull-selector,
  recursively applying to nested maps.

  `pull-selector` takes the same shape as Datomic's pull selector and similar opts:
  `:xform` - a function that will be applied to the corresponding value
  `:as` - replaces the respective key with the provided value
  `:default` - default value to use if a value isn't found for the respective key"
  [m pull-selector]
  (let [pull-opt? #{:as :xform :default}
        opts?     (every-pred (some-fn vector? list?)
                              #(filter pull-opt? %))
        k?        (some-fn (complement coll?)
                           opts?)]
    (reduce (fn [acc k-or-kopts]
              (let [k-fn (fn [k-or-kopts*]
                           (if (opts? k-or-kopts*)
                             (let [[k & {:keys [as]}] k-or-kopts*]
                               (or as k))
                             k-or-kopts*))
                    v-fn (fn [m* k-or-kopts*]
                           (if (opts? k-or-kopts*)
                             (let [[k & {:keys [xform default]}] k-or-kopts*]
                               (cond-> m*
                                 (some? k)
                                 (get k default)

                                 (some? xform)
                                 (xform)))
                             (get m* k-or-kopts*)))]
                (cond
                  (k? k-or-kopts)
                  (if-some [v (v-fn m k-or-kopts)]
                    (assoc acc (k-fn k-or-kopts) v)
                    acc)

                  (map? k-or-kopts)
                  (reduce-kv (fn [acc* k-or-kopts* selector]
                               (let [k (k-fn k-or-kopts*)]
                                 (if-some [v* (pull-keys (v-fn m k-or-kopts*) selector)]
                                   (assoc acc* k v*)
                                   acc*)))
                             acc
                             k-or-kopts))))
            {}
            pull-selector)))

(comment
  ;; xform to-datomic
  ;; xform from-datomic
  ;; generative tests
  ;; fn for schema -> selector w/ xforms

  (pull-keys {:test/level      0
              :test/other-next {:test/level 10
                                :test/names ["name1" "name2"]}
              :test/_students  [{:teacher/id 2 :teacher/name "level 2"}]
              :some.other/foo  "bar"
              :test/next       {:test/level 1
                                :test/name  "level 1"
                                :test/next  {:test/level 2
                                             :test/next  {:test/level 3}}}}

             [:test/level
              [:test/_students :as :test/teacher :xform first]
              [:some.other/foo :as :test/foo]
              {:test/other-next [[:test/names :xform #(map keyword %)]]
               :test/next       [[:test/level :as "nested-level"]
                                 [:test/include? :default false]]}]
             #_[[:test/level :xform str :as "level"]
                [:test/include? :default false]
                {:test/other-next        [[:test/names :xform #(map clojure.string/upper-case %)]]
                 [:test/next :as "next"] [:test/name
                                          [:test/include? :default true]
                                          {:test/next [{:test/next [:test/level]}]}]}])
  )