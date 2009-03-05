;; Copyright (c) James Reeves. All rights reserved.
;; The use and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which
;; can be found in the file epl-v10.html at the root of this distribution. By
;; using this software in any fashion, you are agreeing to be bound by the
;; terms of this license. You must not remove this notice, or any other, from
;; this software.

;; fact.random-utils:
;; 
;; Useful functions for creating random test data.

(ns fact.random-utils)

(defn random-int
  "Generate a random integer, optionally in a specific range."
  ([]
    (random-int -65535 65535))
  ([min max]
    (+ (rand-int (- (inc max) min)) min)))

(defn random-choice
  "Randomly choose an element from a collection."
  [coll]
  (let [v (vec coll)]
    (v (rand-int (count v)))))

(defn random-seq
  "Generate a random sequence from by repeatedly applying a function f."
  ([f]
    (random-seq f 0 100))
  ([f min max]
    (take (random-int min max) (repeatedly f))))

(def ascii-lower   "abcdefghijklmnopqrstuvwxyz")
(def ascii-upper   "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def ascii-letters (str ascii-upper ascii-lower))
(def digits        "01234567890")
(def punctuation   ".,:;!?\"'`<>()[]{}-")
(def white-space   " \t\n")

(defn random-str
  "Generate a random string."
  ([]
    (random-str
      (str ascii-letters digits punctuation white-space)))
  ([chars]
    (random-str chars 0 100))
  ([chars min max]
    (apply str
      (random-seq #(random-choice chars) min max))))
