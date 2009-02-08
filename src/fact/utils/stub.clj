(ns fact.utils.stub)

(defmacro stub
  "Create function stubs for isolated unit tests.
  e.g. (stub [(f 1 2) 3
              (f 3 2) 5]
         (= (+ (f 1 2) (f 3 2))
            8))"
  [stubs & body]
  (let [stub-pairs (partition 2 stubs)
        fold-map   (fn [acc [[f & args] ret]]
                      (assoc-in acc [f `(list ~@args)] ret))
        bind-stub  (fn [[fname fhash]]
                     `(~fname (fn [& args#] (~fhash args#))))]
    `(binding
       [~@(mapcat bind-stub (reduce fold-map {} stub-pairs))]
       ~@body)))
