;; even_squares.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given an integer 0 < n < 10000, print all of the positive even perfect
;; squares < n on separate lines.
;;
;; input stack has input integer n

(ns clojush.problems.software.even-squares
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        [clojure.math numeric-tower])
  (:require [clojure.string :as string]))

; Atom generators
(def even-squares-atom-generators
  (concat (list
            ;;; end ERCs
            (tag-instruction-erc [:integer :boolean :exec] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :exec :print])))

(defn make-even-squares-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-even-squares-error-function
    ([individual]
      (the-actual-even-squares-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-even-squares-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (flatten
                     (doall
                       (for [[input1 correct-output correct-integers] (case data-cases
                                                                          :train train-cases
                                                                          :test test-cases
                                                                          [])]
                         (let [final-state (run-push (:program individual)
                                                     (->> (make-push-state)
                                                       (push-item input1 :input)
                                                       (push-item "" :output)))
                               result (stack-ref :output 0 final-state)]
                           (when print-outputs
                             (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str result))))
                           ; Record the behavior
                           (swap! behavior conj result)
                           (let [correct-number-lines (count correct-integers)
                                 result-lines (if (= result "")
                                                []
                                                (string/split-lines result))
                                 int-parse-strings (filter #(re-matches #"-?\d+" %) result-lines)
                                 lines-with-integer-parseable-strings (count int-parse-strings)
                                 lines-without-integer-parseable-strings (- (count result-lines) lines-with-integer-parseable-strings)]
                             (vector
                               ; Error 1: Levenshtein distance of printed strings
                               (levenshtein-distance correct-output result)
                               ; Error 2: Difference in number of lines with integer-parseable strings. Also, each line without an integer-parseable string contributes 1 error
                               (+ (abs (- correct-number-lines lines-with-integer-parseable-strings))
                                  lines-without-integer-parseable-strings)
                               ; Error 3: For each line in the result with a parseable integer, find the integer error compared to correct integer. Sum these.
                               (let [correct-result-int-pairs (map vector
                                                                   correct-integers
                                                                   (concat (map (fn [int-str]
                                                                                  (try (Integer/parseInt int-str)
                                                                                    (catch Exception e :no-result)))
                                                                                int-parse-strings)
                                                                           (repeat :no-result)))]
                                 (apply +' (map (fn [[cor-int res-int]]
                                                  (if (not (number? res-int))
                                                    100 ; penalty for not enough lines with parseable integers
                                                    (abs (- cor-int res-int))))
                                                correct-result-int-pairs)))))))))]
        (if (= data-cases :train)
          (assoc individual :behaviors @behavior :errors errors)
          (assoc individual :test-errors errors))))))

; Define train and test cases
(def even-squares-train-and-test-cases
  (map #(sort-by first %)
    (train-and-test-cases-from-dataset "even-squares" 83 1000)))

(defn even-squares-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first even-squares-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second even-squares-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn even-squares-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Even Squares problem report - generation %s\n" generation)(flush)
    (println "Test total error for best:" best-total-test-error)
    (println (format "Test mean error for best: %.5f" (double (/ best-total-test-error (count best-test-errors)))))
    (when (zero? (:total-error best))
      (doseq [[i error] (map vector
                             (range)
                             best-test-errors)]
        (println (format "Test Case  %3d | Error: %s" i (str error)))))
    (println ";;------------------------------")
    (println "Outputs of best individual on training cases:")
    (error-function best :train true)
    (println ";;******************************")
    )) ;; To do validation, could have this function return an altered best individual
       ;; with total-error > 0 if it had error of zero on train but not on validation
       ;; set. Would need a third category of data cases, or a defined split of training cases.


; Define the argmap
(def argmap
  {:error-function (make-even-squares-error-function-from-cases (first even-squares-train-and-test-cases)
                                                                (second even-squares-train-and-test-cases))
   :atom-generators even-squares-atom-generators
   :max-points 1600
   :max-genome-size-in-initial-program 200
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:alternation 0.2
                                    :uniform-mutation 0.2
                                    :uniform-close-mutation 0.1
                                    [:alternation :uniform-mutation] 0.5
                                    }
   :alternation-rate 0.01
   :alignment-deviation 10
   :uniform-mutation-rate 0.01
   :problem-specific-report even-squares-report
   :problem-specific-initial-report even-squares-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000
   })
