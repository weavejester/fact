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

(ns fact.random-utils
  (:use clojure.contrib.seq-utils))

(defn random-int
  "Generate a random integer, optionally in a specific range."
  ([]        (random-int -65535 65535))
  ([min max] (+ (rand-int (- (inc max) min)) min)))

(defn random-seq
  "Generate a random sequence by repeatedly applying a function f."
  ([f]         (random-seq f 0 100))
  ([f min max] (take (random-int min max) (repeatedly f))))

(defn random-vec
  "Generate a random vector by repeatedly applying a function f."
  ([f]         (vec (random-seq f)))
  ([f min max] (vec (random-seq f min max))))

(defn random-map
  "Generate a random map using a key function and a value function."
  ([keyf valf]
    (random-map keyf valf 0 100))
  ([keyf valf min max]
    (into {}
      (random-seq (fn [] [(keyf) (valf)]) min max))))

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
      (random-seq #(rand-elt chars) min max))))

(def random-string random-str)

(defn random-keyword
  "Generate a random keyword."
  ([]              (random-keyword (str ascii-letters digits "-_")))
  ([chars]         (keyword (random-str chars)))
  ([chars min max] (keyword (random-str chars min max))))
