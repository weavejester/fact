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
  (:use clojure.contrib.command-line)
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

(defn print-results
  "Print the fact results using the specified output library."
  [output title results]
  (let [namespace (symbol (str "fact.output." output))]
    (require namespace)
    (let [printer (var-get (ns-resolve namespace 'print-results))]
      (printer title results))))

(defn load-facts
  "Load and verify facts from a file, returning the namespace and results."
  [file]
  (load-file file)
  (let [ns (namespace-from-path file)]
    [ns (verify-facts ns)]))

(defn load-result-map
  "Load all facts in a list of files, and return a map of namespaces to
  results."
  [files]
  (into (sorted-map) (map load-facts files)))

(defn print-result-map
  "Load the files with facts in them, verify the facts, and print the results."
  [output result-map]
  (doseq [[ns results] result-map]
    (when-not (empty? results)
      (print-results output ns results))))

(defn -main [& args]
  "Main method."
  (with-command-line args
    "fact - Verify facts in files"
    [[output o "The output type to use"]
     files]
    (let [result-map (load-result-map files)
          results    (mapcat val result-map)]
      (print-result-map
        (or output "verbose")
        result-map)
      (when (some exception? results)
        (System/exit 2))
      (when (some failure? results)
        (System/exit 1)))))
