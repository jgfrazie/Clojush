;; double_letters.clj
;; Tom Helmuth, thelmuth@cs.umass.edu
;;
;; Problem Source: iJava (http://ijava.cs.umass.edu/)
;;
;; Given a string, print the string, doubling every letter character, and
;; trippling every exclamation point. All other non-alphabetic and non-exclamation
;; characters should be printed a single time each. The input string will have
;; maximum length of 20 characters.
;;
;; input stack has the input string

(ns clojush.problems.software.double-letters
  (:use clojush.pushgp.pushgp
        [clojush pushstate interpreter random util globals]
        clojush.instructions.tag
        clojure.math.numeric-tower
        )
  (:require [clojush.pushgp.case-auto-generation :as cag]))

; Atom generators
(def double-letters-atom-generators
  (concat (list
            \!
            ;;; end constants
            ;;; end ERCs
            (tag-instruction-erc [:exec :integer :boolean :string :char] 1000)
            (tagged-instruction-erc 1000)
            ;;; end tag ERCs
            'in1
            ;;; end input instructions
            )
          (registered-for-stacks [:integer :boolean :string :char :exec :print])))


;; Define test cases
(defn double-letters-input
  "Makes a Double Letters input of length len."
  [len]
  (apply str
         (repeatedly len
                     #(lrand-nth (concat [\newline \tab]
                                         (map char (range 32 127)))))))

;; A list of data domains for the problem. Each domain is a vector containing
;; a "set" of inputs and two integers representing how many cases from the set
;; should be used as training and testing cases respectively. Each "set" of
;; inputs is either a list or a function that, when called, will create a
;; random element of the set.
(def double-letters-data-domains
  [[(list "", "A", "!", " ", "*", "\t", "\n", "B\n", "\n\n", "CD", "ef", "!!", "q!", "!R", "!#", "@!", "!F!", "T$L", "4ps", "q\t ", "!!!"
          (apply str (take 13 (cycle (list \i \: \!))))
          (apply str (repeat 20 \8))
          (apply str (repeat 20 \space))
          (apply str (repeat 20 \s))
          (apply str (repeat 20 \!))
          (apply str (take 20 (cycle (list \H \a \space))))
          (apply str (take 20 (cycle (list \x \newline \y \!))))
          (apply str (take 20 (cycle (list \1 \!))))
          (apply str (take 20 (cycle (list \G \5))))
          (apply str (take 20 (cycle (list \> \_ \= \]))))
          (apply str (take 20 (cycle (list \k \! \!))))) 32 0] ;; "Special" inputs covering some base cases
   [(fn [] (double-letters-input (inc (lrand-int 20)))) 68 1000]
   ])

;;Can make Double Letters test data like this:
;(test-and-train-data-from-domains double-letters-data-domains)

; Helper function for error function
(defn double-letters-test-cases
  "Takes a sequence of inputs and gives IO test cases of the form
   [input output]."
  [inputs]
  (map (fn [in]
         (vector [in]
                 [(apply str (flatten (map #(cond
                                             (Character/isLetter %) (list % %)
                                             (= % \!) (list % % %)
                                             :else %)
                                          in)))]))
       inputs))

(defn double-letters-solver
  "Given a string, print the string, doubling every letter character, and
   trippling every exclamation point. All other non-alphabetic 
   and non-exclamation characters should be printed a single 
   time each."
  [input]
  (apply str (flatten (map #(cond
                              (Character/isLetter %) (list % %)
                              (= % \!) (list % % %)
                              :else %)
                           input))))

(defn make-double-letters-error-function-from-cases
  [train-cases test-cases]
  (fn the-actual-double-letters-error-function
    ([individual]
      (the-actual-double-letters-error-function individual :train))
    ([individual data-cases] ;; data-cases should be :train or :test
     (the-actual-double-letters-error-function individual data-cases false))
    ([individual data-cases print-outputs]
      (let [behavior (atom '())
            errors (doall
                     (for [[[input] [correct-output]] (case data-cases
                                                    :train train-cases
                                                    :test test-cases
                                                    data-cases)]
                       (let [final-state (run-push (:program individual)
                                                   (->> (make-push-state)
                                                     (push-item input :input)
                                                     (push-item "" :output)))
                             printed-result (stack-ref :output 0 final-state)]
                         (when print-outputs
                           (println (format "| Correct output: %s\n| Program output: %s\n" (pr-str correct-output) (pr-str printed-result))))
                         ; Record the behavior
                         (swap! behavior conj printed-result)
                         ; Error is Levenshtein distance
                         (levenshtein-distance correct-output printed-result))))]
        (if (= data-cases :test)
          (assoc individual :test-errors errors)
          (assoc individual :behaviors @behavior :errors errors)
          )))))

(defn get-double-letters-train-and-test
  "Returns the train and test cases."
  [data-domains]
  (map #(sort-by (comp count first) %)
       (map double-letters-test-cases
            (test-and-train-data-from-domains data-domains))))

; Define train and test cases
(def double-letters-train-and-test-cases
  (get-double-letters-train-and-test double-letters-data-domains))

(defn double-letters-initial-report
  [argmap]
  (println "Train and test cases:")
  (doseq [[i case] (map vector (range) (first double-letters-train-and-test-cases))]
    (println (format "Train Case: %3d | Input/Output: %s" i (str case))))
  (doseq [[i case] (map vector (range) (second double-letters-train-and-test-cases))]
    (println (format "Test Case: %3d | Input/Output: %s" i (str case))))
  (println ";;******************************"))

(defn double-letters-report
  "Custom generational report."
  [best population generation error-function report-simplifications]
  (let [best-test-errors (:test-errors (error-function best :test))
        best-total-test-error (apply +' best-test-errors)]
    (println ";;******************************")
    (printf ";; -*- Double Letters problem report - generation %s\n" generation)(flush)
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
  {:error-function (make-double-letters-error-function-from-cases (first double-letters-train-and-test-cases)
                                                                  (second double-letters-train-and-test-cases))
   :training-cases (first double-letters-train-and-test-cases)

   :sub-training-cases-selection :intelligent ; :random ; :intelligent
   :num-of-cases-in-sub-training-set 5
   :num-of-edge-cases-in-sub-training-set 2 ; probably not 5 since there's only 1 input
   :sub-training-cases '()
   :atom-generators double-letters-atom-generators

    ;; Human-driven counterexamples
   :counterexample-driven true
   :counterexample-driven-case-checker :simulated-human ; :automatic ; :human ; :simulated-human

   ;; Options, as a list: :hard-coded ; :randomly-generated ; :edge-cases ; :selecting-new-cases-based-on-outputs
   :counterexample-driven-case-generators '(:edge-cases :branch-coverage-test :selecting-new-cases-based-on-outputs :randomly-generated)

   :max-num-of-cases-added-from-edge 2
   :num-of-cases-added-from-random 8
   
   :num-of-cases-used-for-output-selection 100
   :num-of-cases-added-from-output-selection 5
   :num-of-cases-used-for-branch-coverage 100
   :num-of-cases-added-from-branch-coverage 5

   :input-parameterization [(cag/create-new-parameter :string 0 20 [:digits :lower-case :upper-case :specials] [])]
   :output-stacks [:output]
   :oracle-function double-letters-solver

   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1.0}
   :uniform-addition-and-deletion-rate 0.09
   :problem-specific-report double-letters-report
   :problem-specific-initial-report double-letters-initial-report
   :report-simplifications 0
   :final-report-simplifications 5000
   :max-error 5000})
