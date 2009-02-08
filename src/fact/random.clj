(ns fact.random)

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

(defmulti
  #^{:doc "Return a random object of a particular class."}
  random-object identity)

(defmethod random-object Integer [_] (random-int))
(defmethod random-object String  [_] (random-str))
