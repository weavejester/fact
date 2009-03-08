;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; fact.output.color:
;; 
;; Verbose color output of fact results.

(ns fact.output.color
  (:use fact.core)
  (:use [fact.output.verbose :only (format-result print-summary)]))

(def ansi-red     "\033[31m")
(def ansi-green   "\033[32m")
(def ansi-brown   "\033[33m")
(def ansi-default "\033[0m")

(defn print-results
  "Print the results from a set of verified facts... in COLOR!
  (Requires a shell with ANSI-compatible colors)"
  [title results]
  (println title)
  (doseq [result results]
    (cond
      (pending? result)   (print ansi-brown)
      (exception? result) (print ansi-red)
      (failure? result)   (print ansi-red)
      :fact-passed        (print ansi-green))
    (print (format-result result))
    (print ansi-default)
    (println))
  (print-summary results))
