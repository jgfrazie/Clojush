;; human_driven.clj
;; Tom Helmuth, thelmuth@hamilton.edu
;;
;; Implementation of human-interactive program synthesis
;;
;; The user needs to specify:
;; - The input numbers, types, ranges
;; - The output numbers, types
;; - An initial set of training cases
;; - A few things related to the atom-generators
;;
;; We'll assume we can create, for now, but might need to be user input:
;; - Error function based on type of output(s)
;;

(ns clojush.problems.software.human-driven
  (:require [clojush random pushstate interpreter]
            clojush.pushgp.case-auto-generation :as cag))

;; Atom generators
;; This needs tons of work
;; For example, the user should be able to specify the stacks to use for registered-for-stacks,
;;   the number of inputs, and any constants (or ERCs?) to include.
;; Also, if there are multiple outputs, need to have output instructions.
(def human-driven-atom-generators
  (concat (list
            (fn [] (- (clojush.random/lrand-int 21) 10))
            ;;; end tag ERCs
            'in1
            'in2
            ;;; end input instructions
            )
          (clojush.pushstate/registered-for-stacks [:integer :boolean :vector_integer :exec])))

(def input-parameterization (cag/acquire-parameters-from-user))

(def output-parameterization (cag/acquire-outputs-from-user))

(def initial-training-cases (cag/get-initial-training-cases-from-user))

;; TMH: This function is likely done, but not tested.
(defn human-driven-evaluate-program-for-behaviors
  "Evaluates the program on the given list of cases.
   Returns the behaviors, a list of the outputs of the program on the inputs."
  [program cases output-types]
  (doall
   (for [[input output] cases]
     (let [reversed-inputs (reverse input)
           final-state (clojush.interpreter/run-push
                        program
                        (reduce (fn [psh-state inp]
                                  (clojush.pushstate/push-item psh-state inp :input))
                                (clojush.pushstate/make-push-state)
                                reversed-inputs))]
       (map #(clojush.pushstate/top-item % final-state)
            output-types)))))

;; TMH: this function is not implemented. Will need to decide how to evaluate
;; error based on the output data types
(defn human-driven-errors-from-behaviors
  "Takes a list of behaviors across the list of cases and finds the error
   for each of those behaviors, returning an error vector."
  [behaviors cases output-types]
  (map (fn [result correct-output]
         (if (= result correct-output)
           0
           1))
       behaviors
       (map second cases)))

;; TMH: Not sure if this function's done or not.
(defn human-driven-error-function
  "The error function. Takes an individual and data-cases as input,
   and returns that individual with :errors and :behaviors set.
   data-cases should be a list of input/output pairs. For consistency between
   problems with single inputs and multiple inputs, each input should be a sequence
   containing one or more inputs, and same with outputs."
  [individual data-cases output-types]
  (let [behaviors (human-driven-evaluate-program-for-behaviors (:program individual)
                                                               data-cases
                                                               output-types)
        errors (human-driven-errors-from-behaviors behaviors data-cases output-types)]
    (assoc individual
           :behaviors behaviors
           :errors errors)))



; Define the argmap
(def argmap
  {:error-function human-driven-error-function
   :input-parameterization input-parameterization
   :output-parameterization output-parameterization
   :training-cases initial-training-cases
   :atom-generators human-driven-atom-generators
   :max-points 2000
   :max-genome-size-in-initial-program 250
   :evalpush-limit 2000
   :population-size 1000
   :max-generations 300
   :parent-selection :lexicase
   :genetic-operator-probabilities {:uniform-addition-and-deletion 1.0}
   :uniform-addition-and-deletion-rate 0.09
   :report-simplifications 0
   :final-report-simplifications 5000})
