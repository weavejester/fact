(ns fact.random)

(defn rand-ints
  "Generate an infinite sequence of random integers."
  ([]
    (rand-ints -65535 65535))
  ([min max]
    (repeatedly #(+ (rand-int (- max min)) min))))

(defn rand-elems
  "Generate an infinite sequence of random elements from a collection."
  [coll]
  (let [v (vec coll)]
    (repeatedly
      #(v (rand-int (count v))))))

(defn rand-seqs
  "Generate an infinite sequence of random length sequences created by
  a function f."
  ([f]
    (rand-seqs f 0 100))
  ([f min max]
    (map #(take % (f))
          (rand-ints min max))))

(def ascii-lower   "abcdefghijklmnopqrstuvwxyz")
(def ascii-upper   "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def ascii-letters (str ascii-upper ascii-lower))
(def digits        "01234567890")
(def punctuation   ".,:;!?\"'`<>()[]{}-")
(def white-space   " \t\n")

(defn rand-strs
  "Generate a sequence of random strings."
  ([]
    (rand-strs
      (str ascii-letters digits punctuation white-space)))
  ([chars]
    (rand-strs chars 0 100))
  ([chars min max]
    (map (partial apply str)
         (rand-seqs #(rand-elems chars) min max))))
