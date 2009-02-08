(ns fact.output.verbose)

(defn- format-params
  "Format a collection of parameters and values to a string."
  [params values]
  (apply str
    (interpose ", "
      (map #(str %1 " = " (pr-str %2))
           params
           values))))

(defn- format-exception
  "Format an exception thrown by a test as a string."
  [result]
  (let [[exception values] (first (result :exceptions))]
    (str "\n  EXCEPTION WHEN: "
      (format-params ((result :fact) :params) values)
      "\n  => " exception)))

(defn- format-failure
  "Format a failed result as a string."
  [result]
  (str "\n  FAILURE WHEN: "
    (format-params ((result :fact) :params)
                   (first (result :failures)))))

(defn- failure?
  "Did the fact fail?"
  [result]
  (seq (result :failures)))

(defn- exception?
  "Did the fact throw an exception?"
  [result]
  (seq (result :exceptions)))

(defn- pending?
  "Is the fact pending a verification test?"
  [result]
  (:pending? (result :fact)))

(defn- format-result
  "Format a single result from a verified fact."
  [result]
  (str "- " (:doc (result :fact))
            (cond
              (pending? result)   " (pending)"
              (exception? result) (format-exception result)
              (failure? result)   (format-failure result))))

(defn- print-summary
  "Print a summary of how many facts succeeded, failed, or excepted."
  [results]
  (println (count results) "facts,"
           (count (filter pending? results)) "pending,"
           (count (filter failure? results)) "failed,"
           (count (filter exception? results)) "exceptions"))

(defn print-results
  "Prints the results from a set of verified facts to *test-out*."
  [results]
  (doseq [result results]
    (println (format-result result)))
  (print-summary results))

(def ansi-red     "\033[31m")
(def ansi-green   "\033[32m")
(def ansi-brown   "\033[33m")
(def ansi-default "\033[0m")

(defn print-color-results
  "Print the results from a set of verified facts... in COLOR!
  (Requires a shell with ANSI-compatible colors)"
  [results]
  (doseq [result results]
    (cond
      (pending? result)   (print ansi-brown)
      (exception? result) (print ansi-red)
      (failure? result)   (print ansi-red)
      :fact-passed        (print ansi-green))
    (print (format-result result))
    (print  ansi-default)
    (println))
  (print-summary results))
