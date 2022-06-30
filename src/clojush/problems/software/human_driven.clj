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
;; NOTE: Need to be careful if use this on any PSB1 problems or Index Of
;;       Substrings from PSB2, since they might use a non-standard error function.

;; Anywhere cases are used, they're expected to be of the form:
;; ([[input1 input2 ...] [output1 output2 ...]], ;; first case
;;  [[input1 input2 ...] [output1 output2 ...]], ;; second case
;;    ...)
;; This is even true if there's only 1 input or 1 output, i.e. each case
;; is still a vector containing a vector as the input(s) and a vector as the output(s)

(ns clojush.problems.software.human-driven
  (:require [clojush random pushstate interpreter util]
            [clojush.pushgp.case-auto-generation :as cag]
            [clojure.math.numeric-tower :as math]))

;; Penalty error used if no answer is returned by program
(def penalty-error 1000000)

(def input-parameterization (cag/acquire-parameters-from-user))

(def output-types (cag/acquire-outputs-from-user))

(def initial-training-cases (cag/get-initial-training-cases-from-user
                             input-parameterization
                             output-types
                             5))

;; Atom generators
;; This needs tons of work
;; For example, the user should be able to specify the stacks to use for registered-for-stacks,
;;   the number of inputs, and any constants (or ERCs?) to include.
;; Also, if there are multiple outputs, need to have output instructions.
(def human-driven-atom-generators
  (concat (list
           (fn [] (- (clojush.random/lrand-int 21) 10))
           ""
            ;;; end tag ERCs
           )
          (cag/acquire-input-instructions input-parameterization)
          (clojush.pushstate/registered-for-stacks (cag/acquire-atom-generator-push-stacks))))

(defn human-driven-evaluate-program-for-behaviors
  "Evaluates the program on the given list of cases.
   Returns the behaviors, a list of the outputs of the program on the inputs."
  [program cases]
  (doall
   (for [[input output] cases]
     (let [reversed-inputs (reverse input)
           start-state (reduce (fn [psh-state inp]
                                 (clojush.pushstate/push-item inp :input psh-state))
                               (clojush.pushstate/make-push-state)
                               reversed-inputs)
           final-state (clojush.interpreter/run-push
                        program
                        start-state)]
       (map #(clojush.pushstate/top-item % final-state)
            output-types)))))

;; Map of error functions by type
(def error-distance-functions
  {:boolean (fn [x y] (if (= x y) 0 1))
   :integer (fn [x y] (math/abs (- x y)))
   :float (fn [x y] (math/abs (- x y)))
   :string clojush.util/levenshtein-distance
   :vector_integer cag/vector-of-number-difference
   :vector_float cag/vector-of-number-difference})

(defn human-driven-errors-from-behaviors
  "Takes a list of behaviors across the list of cases and finds the error
   for each of those behaviors, returning an error vector.
   Note: each behavior must be a sequence, even if only one output"
  [behaviors cases]
  (flatten
   ;; Outer map over all training cases
   (map (fn [result-vector correct-output-vector]
          ;; Inner map over outputs of a single case (often only one, but could be multiple)
          (map (fn [out-type result correct-output]
                 (if (= result :no-stack-item)
                   penalty-error
                   ((get error-distance-functions out-type) result correct-output)))
               output-types
               result-vector
               correct-output-vector))
        behaviors
        (map second cases))))

(defn human-driven-error-function
  "The error function. Takes an individual and data-cases as input,
   and returns that individual with :errors and :behaviors set."
  [individual data-cases]
  (let [behaviors (human-driven-evaluate-program-for-behaviors (:program individual)
                                                               data-cases)
        errors (human-driven-errors-from-behaviors behaviors data-cases)]
    (assoc individual
           :behaviors behaviors
           :errors errors)))


; Define the argmap
(def argmap
  {:error-function human-driven-error-function
   :input-parameterization input-parameterization
   :output-stacks output-types
   :training-cases (list
                    [[5 "Hamilton"] ["Hamil"]]
                    [[17 "computer"] ["computer"]]
                    [[2 "cheese"] ["ch"]]
                    [[5 "gebna"] ["gebna"]])

   :sub-training-cases initial-training-cases
   :atom-generators human-driven-atom-generators

   ;; TMH: Add some pushargs here to do the counterexamples correctly
   :counterexample-driven true
   :counterexample-driven-case-generator :randomly-generated ; :hard-coded ; :auto-generated ; :randomly-generated ; :edge-cases ; :selecting-new-cases-based-on-outputs
   :counterexample-driven-case-checker :human ; :automatic ; :human

   :num-of-cases-used-for-output-selection 1000
   :num-of-cases-added-from-output-selection 5

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


;; TMH NOTE: Testing problem
;; Given in1 = Integer [0, 20]
;; and   in2 = String length [0, 20], all chars
;;      out1 = String
;; Return the substring containing the first in1 characters of in2

(comment

  (def prog '(in1 in2 integer_add))
  (def prog2 '("dwdwadwa"))

  (def some-cases '([[1 2] [2]]
                    [[3 7] [21]]
                    [[-3 5] [-15]]))

  (def out-types [:integer])

  ;; test human-driven-evaluate-program-for-behaviors
  (human-driven-evaluate-program-for-behaviors prog some-cases)
  ;; => ((3) (10) (2))

  (human-driven-errors-from-behaviors (human-driven-evaluate-program-for-behaviors
                                       prog
                                       some-cases)
                                      some-cases)
  ;; => (1 11 17)

  (def prog-str '(in2 in1 string_concat [] in1 string_length vector_integer_conj
                      in2 string_length vector_integer_conj))

  (def string-cases '([["hi" "there"] ["hithere" [2 5]]]
                      [["123" "456"] ["123456" [3 3]]]
                      [["" ""] ["" [0 0]]]))

  (def out-types-str [:string :vector_integer])

  (human-driven-evaluate-program-for-behaviors prog-str string-cases)

  (human-driven-errors-from-behaviors (human-driven-evaluate-program-for-behaviors
                                       prog-str
                                       string-cases)
                                      string-cases)

  (human-driven-error-function {:program prog} some-cases)
  
  )
