(ns synthesis.metrics
  (:require [synthesis.examples_tables :as et]
            [synthesis.db :as db]
            [synthesis.qfe :as qfe]))


;; Setup examples table
(et/drop-examples-table)
(et/create-and-populate-examples-table et/pos-ex et/neg-ex)

;; Desired query Q0 WHERE clause
(def Q0-where
  "(greater_50k = '>50K')")

;; Evolved solution query Q WHERE clause
(def Q-where
  "((19 <= age AND age <= 90) AND ('?' <= workclass AND workclass <= 'State-gov') AND (14878 <= fnlwgt AND fnlwgt <= 1226583) AND ('10th' <= education AND education <= 'Some-college') AND (2 <= education_num AND education_num <= 16) AND ('Divorced' <= marital_status AND marital_status <= 'Widowed') AND ('?' <= occupation AND occupation <= 'Transport-moving') AND ('Husband' <= relationship AND relationship <= 'Wife') AND ('Amer-Indian-Eskimo' <= race AND race <= 'White') AND ('Female' <= sex AND sex <= 'Male') AND (0 <= capital_gain AND capital_gain <= 99999) AND (0 <= capital_loss AND capital_loss <= 3683) AND (1 <= hours_per_week AND hours_per_week <= 99) AND ('?' <= native_country AND native_country <= 'Yugoslavia'))")


;; Precision, recall, and f1-score of Q-where 

(defn print-metrics
  [database-string where-clause positive-examples negative-examples]
  (let [result-query-string (str "SELECT *\nFROM "
                                 database-string
                                 "\nWHERE "
                                 where-clause)
        query-future (future
                       (db/run-db-function db/synthesis-db
                                           db/db-query
                                           result-query-string))]
    (println "\n\nQuery:")
    (println result-query-string)
    (try
      (let [result-rows (.get query-future 2000
                              (java.util.concurrent.TimeUnit/MILLISECONDS))
            true-positives (count (clojure.set/intersection (set positive-examples)
                                                            (set result-rows)))
            false-positives (count (clojure.set/intersection (set negative-examples)
                                                             (set result-rows)))
            true-negatives (- (count negative-examples) false-positives)
            false-negatives (- (count positive-examples) true-positives)
            error (- 1.0 (qfe/f1-score true-positives
                                       false-positives
                                       (- (count positive-examples) true-positives)))]
        (println "\nTotal Actual Positives: " (count positive-examples))
        (println "Total Actual Negatives: " (count negative-examples))
        (println "Total: " (+ (count negative-examples) (count positive-examples)))
        (println "\nTrue positives: " true-positives)
        (println "True negatives: " true-negatives)
        (println "False negatives: " false-negatives)
        (println "False positives: " false-positives)
        (println "\nAccuracy: " (float (/ (+ true-positives true-negatives)
                                          (+ (count negative-examples) (count positive-examples)))))
        (println "Precision: " (qfe/precision true-positives false-positives))
        (println "Recall: " (qfe/recall true-positives false-negatives))
        (println "F1-Score: " (qfe/f1-score true-positives false-positives false-negatives) "\n")
        nil)
      (catch java.util.concurrent.TimeoutException e
             (when (not (future-cancel query-future))
               (println "future could not be cancelled"))
             nil))))

;; Print metrics over examples
(print-metrics "adult_examples" Q-where et/pos-ex et/neg-ex)

;; Print metrics over entire table
(print-metrics "adult"
               Q-where
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult
                                         WHERE "
                                        Q0-where))
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult
                                         WHERE NOT("
                                        Q0-where
                                        ")")))

;; Print metrics over test data table
(print-metrics "adult_test"
               Q-where
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult_test
                                         WHERE "
                                        Q0-where))
               (db/run-db-function db/synthesis-db
                                   db/db-query
                                   (str "SELECT *
                                         FROM adult_test
                                         WHERE NOT("
                                        Q0-where
                                        ")")))
