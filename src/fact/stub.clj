;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; fact.stub:
;; 
;; Stub existing functions.

(ns fact.stub)

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
