;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; fact.main
;;
;; Main method to finds and verify all facts in a directory.

(ns fact.main
  (:gen-class)
  (:use fact.core)
  (:use fact.output.verbose)
  (:import java.io.File))

(defn resource-name
  "Returns the resource name of the Clojure file, or nil if the file does not
  have the .clj extension."
  [file]
  (second (re-matches #"(.*)\.clj" file)))

(defn namespace-from-path
  "Given a path to a resource, return a symbol of the expected namespace."
  [file]
  (symbol
    (.. (resource-name file)
      (replace File/separator ".")
      (replace "_" "-"))))

(defn -main [& files]
  (doseq [file files]
    (println file)
    (load-file file)
    (let [ns    (namespace-from-path file)
          facts (verify-facts ns)]
      (when-not (empty? facts)
        (println ns)
        (print-results facts)
        (println)))))
