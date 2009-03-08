;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; fact.output.quiet:
;; 
;; No output whatsoever. Useful for scripts that just work off the exit status.

(ns fact.output.quiet)

(defn print-results
  "Doesn't output anything."
  [title results]
  nil)
