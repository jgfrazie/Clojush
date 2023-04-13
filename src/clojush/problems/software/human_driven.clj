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

;; Atom generators
(def human-driven-atom-generators
  (let [requested-stacks (cag/acquire-atom-generator-push-stacks)]
    (concat (cag/acquire-input-instructions input-parameterization)
            (cag/acquire-atom-generator-constants requested-stacks)
            (clojush.pushstate/registered-for-stacks requested-stacks))))

(def initial-training-cases (cag/get-initial-training-cases-from-user
                             input-parameterization
                             output-types
                             (cag/process-user-input "
*** How many cases would you like to input?***
                                                      " :integer)))

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

(defn gcd-solver
  [in1 in2]
  (loop [a in1 b in2]
    (if (zero? b) a
        (recur b (mod a b)))))


; Define the argmap
(def argmap
  {:error-function human-driven-error-function
   :input-parameterization input-parameterization
   :output-stacks output-types
   :training-cases '() ;; These are ignored by human-driven GP
   :sub-training-cases-selection :intelligent
   :sub-training-cases initial-training-cases ;; These are the cases given by the user.
   :atom-generators human-driven-atom-generators
  ;;  :oracle-function (fn string-soln
  ;;                     [integer string]
  ;;                     (if (< integer (count string))
  ;;                       (subs string 0 integer)
  ;;                       string))

  ;;  :oracle-function gcd-solver
   
   :oracle-function #(clojure.string/includes? % " ")

   ;; Human-driven counterexamples
   :counterexample-driven true
   :counterexample-driven-case-checker :simulated-human ;:human ; :automatic ; :human ; :simulated-human

   ;; Options, as a list: :hard-coded ; :randomly-generated ; :edge-cases ; :selecting-new-cases-based-on-outputs
   :counterexample-driven-case-generators '(:edge-cases :branch-coverage-test :selecting-new-cases-based-on-outputs :randomly-generated)

   :max-num-of-cases-added-from-edge 5
   :num-of-cases-added-from-random 5
   :num-of-cases-used-for-output-selection 1000
   :num-of-cases-added-from-output-selection 5
   :num-of-cases-used-for-branch-coverage 1000
   :num-of-cases-added-from-branch-coverage 5

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


;; NOTE: Testing problem
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
  
  (defn oracle
    [num s]
    (loop [substring ]))
  
  (apply oracle '(20 ">m*o)"))

  )


;;; This program passed initial cases but not later ones
;;; First two programs don't work when answer is a string composed of digits, becase an integer_fromstring instruction steals the answer if so.
(comment
  '(integer_rot string_dup integer_dec boolean_eq integer_mod boolean_and integer_div true integer_empty integer_dup_items char_shove boolean_dup_items string_take boolean_dup_items integer_dup_times string_containschar string_reverse exec_yankdup string_contains integer_abs false string_replacefirst char_eq 1 boolean_or in2 integer_flush integer_fromchar integer_shove string_parse_to_chars string_uppercase string_concat boolean_yankdup boolean_flush string_rest string_pop string_concat char_frominteger true char_eq char_dup integer_mult string_emptystring char_dup_times integer_dec exec_dup_times () char_isletter integer_dec in2 string_frominteger char_shove char_lowercase integer_min integer_dec string_swap string_yankdup string_yank string_conjchar string_take char_iswhitespace char_allfromstring integer_swap boolean_and boolean_pop in1 exec_swap () (char_uppercase string_take) exec_k () () integer_fromstring exec_y (char_flush exec_stackdepth string_setchar integer_gt) char_isdigit string_fromchar boolean_dup_times string_fromboolean exec_while (string_empty exec_shove (exec_swap () (integer_shove exec_empty string_eq boolean_invert_first_then_and integer_pow string_setchar char_isdigit integer_pow boolean_swap 1 string_replacefirstchar integer_rot char_dup_times string_dup_times string_frominteger boolean_or) integer_dup) string_reverse) string_swap string_replace string_dup_items integer_eq boolean_stackdepth integer_gt boolean_eq string_concat string_contains \newline exec_yankdup integer_fromboolean integer_dec boolean_dup string_fromboolean boolean_empty char_iswhitespace boolean_pop string_occurrencesofchar string_first integer_rot boolean_invert_first_then_and char_uppercase char_dup_times integer_stackdepth boolean_dup_times exec_s () (exec_swap (char_allfromstring boolean_invert_second_then_and string_nth string_replacefirstchar integer_flush string_replace string_dup_times char_iswhitespace char_empty string_swap boolean_invert_second_then_and char_flush) (exec_while (string_fromchar char_swap) exec_string_iterate (boolean_yank exec_dup_items char_swap string_conjchar string_containschar boolean_or boolean_empty) exec_do*times () string_fromboolean) string_replace) (boolean_dup_times string_replacechar) integer_mod integer_div integer_fromchar exec_stackdepth boolean_rot integer_div string_indexofchar integer_empty exec_y (string_yankdup string_rest) integer_sub integer_dup_items boolean_yankdup boolean_dup_times boolean_dup_times integer_dec char_pop integer_yankdup \newline exec_y (string_yankdup integer_dup_items) string_eq boolean_pop exec_noop exec_do*times () char_yankdup integer_pow "" string_concat integer_dup_times string_uppercase string_removechar string_last char_pop 0 string_parse_to_chars exec_when (exec_yankdup boolean_yank string_conjchar integer_lt) exec_rot (integer_shove integer_dup_items string_replacefirstchar exec_dup_items char_lowercase) (exec_flush) (string_emptystring string_fromboolean string_replacechar))

  '(integer_rot integer_negate string_dup exec_dup (integer_dec integer_pow boolean_eq boolean_and) integer_mod integer_div true integer_empty integer_dup_items char_shove boolean_dup_items integer_dup_times string_containschar exec_yankdup string_contains integer_abs false string_replacefirst char_eq 1 boolean_or in2 integer_fromchar integer_shove in1 string_uppercase string_concat char_isletter boolean_yankdup boolean_flush string_rest string_concat char_eq char_dup string_removechar integer_mult char_dup_times integer_dec string_pop exec_dup_times () char_isletter integer_dec in2 string_frominteger exec_shove (char_shove integer_yank integer_dup_items char_lowercase integer_dec string_swap string_yankdup string_conjchar string_replacechar) string_take char_iswhitespace integer_swap boolean_and char_lowercase in1 exec_swap () (string_take) string_concat exec_k () () integer_fromstring exec_do*while (exec_y (exec_stackdepth string_setchar integer_gt) char_isdigit boolean_dup_times) string_fromboolean exec_while (exec_shove (exec_if (exec_swap () (boolean_invert_first_then_and integer_shove exec_empty string_eq boolean_invert_first_then_and integer_pow string_setchar true char_isdigit integer_pow boolean_swap 1 string_replacefirstchar integer_rot char_dup_times string_dup_times string_frominteger boolean_or) integer_dup) (string_reverse)) string_swap string_dup_items integer_eq boolean_stackdepth integer_gt) boolean_eq string_concat string_contains integer_fromboolean integer_dec boolean_dup exec_swap (string_fromboolean string_substring string_nth) (boolean_empty boolean_pop char_stackdepth string_occurrencesofchar char_uppercase) integer_rot boolean_invert_first_then_and char_uppercase string_last char_dup_times boolean_dup_times -1 string_dup_times string_rest exec_s () (char_allfromstring string_rest boolean_invert_second_then_and string_substring string_replacefirstchar integer_flush string_dup_times char_iswhitespace char_empty string_swap integer_dup boolean_invert_second_then_and string_parse_to_chars char_flush) (exec_while (char_swap) exec_string_iterate (boolean_yank integer_dup_times char_lowercase) char_swap string_conjchar string_containschar integer_negate boolean_or boolean_empty) exec_do*times () string_replace boolean_dup_times integer_mod integer_div exec_stackdepth boolean_rot integer_empty exec_y (true boolean_dup_times string_yankdup string_rest) string_stackdepth integer_sub integer_dup_items boolean_dup_times boolean_dup_times integer_dec char_pop boolean_empty string_uppercase \newline exec_y (string_yankdup integer_dup_items) boolean_pop exec_noop exec_do*times () char_yankdup string_eq string_concat string_uppercase integer_yank boolean_pop string_removechar char_pop string_parse_to_chars exec_when (integer_lte string_eq string_conjchar integer_lt) exec_rot (integer_shove string_lowercase integer_abs integer_dup_items string_replacefirstchar exec_dup_items char_lowercase) (exec_flush) (string_emptystring string_fromboolean integer_dup string_replacechar))

  ;; This one works, was evolved after giving counterexamples to previous two
  '(integer_rot integer_negate string_dup integer_dec integer_yank boolean_eq boolean_and integer_mod integer_div true integer_empty integer_dup_items string_take boolean_dup_items integer_dup_times true string_containschar string_contains integer_shove false integer_swap 1 false in2 char_frominteger integer_flush integer_shove in1 exec_pop (integer_max string_uppercase string_concat) exec_dup_items string_eq boolean_yankdup boolean_flush string_rest string_pop string_concat char_pop char_frominteger char_dup_times char_eq char_dup string_emptystring char_dup_times string_eq integer_dec string_pop exec_dup_times () char_isletter integer_dec in2 string_frominteger string_yank char_shove char_lowercase integer_dec string_swap string_yankdup string_conjchar boolean_not string_take char_iswhitespace integer_swap boolean_and string_conjchar boolean_pop integer_sub in1 char_uppercase boolean_eq string_take string_concat exec_k () () exec_y (exec_stackdepth exec_flush) string_setchar integer_gt string_shove char_isdigit boolean_dup_times string_fromboolean string_empty exec_shove (exec_swap () (integer_shove exec_empty string_eq string_eq boolean_invert_first_then_and integer_pow string_setchar true true char_frominteger integer_pow 1 string_replacefirstchar integer_rot string_dup_times exec_swap (boolean_or) (integer_dup) string_reverse)) string_swap string_replace string_dup_items integer_eq boolean_stackdepth integer_gt string_contains string_dup_items exec_yankdup integer_fromboolean integer_dec boolean_dup string_fromboolean boolean_empty boolean_pop boolean_flush string_occurrencesofchar boolean_and char_uppercase integer_rot boolean_invert_first_then_and exec_dup_items boolean_dup_times integer_lte exec_s (boolean_invert_second_then_and char_empty integer_flush string_containschar exec_shove (string_replace string_dup_times char_dup_items char_iswhitespace boolean_xor) char_empty string_swap boolean_invert_second_then_and string_parse_to_chars char_flush) (exec_while (boolean_dup_times string_fromchar char_swap) string_setchar char_lowercase) (exec_dup_items char_swap string_containschar boolean_or integer_rot) integer_flush string_fromboolean string_replacechar integer_mod integer_div boolean_swap integer_fromchar integer_stackdepth boolean_rot string_indexofchar integer_empty exec_y (true string_yankdup string_rest) string_includes integer_sub boolean_yankdup boolean_dup_times boolean_dup_times integer_dec \newline exec_y (integer_fromstring) string_yankdup exec_do*times () integer_dup_items string_eq boolean_pop char_dup_times exec_noop integer_shove exec_do*times () char_yankdup "" string_eq string_uppercase string_removechar string_last char_pop integer_dup_times 0 boolean_yankdup string_parse_to_chars integer_lte integer_lt exec_swap (exec_eq exec_rot (string_lowercase integer_dup_items string_replacefirstchar exec_flush) (string_emptystring string_replacechar) ()) ())

  (clojush.util/levenshtein-distance "1" 0)

  )

(comment
  (defn element-rank
    "Given a coll that is a list or vector of integers, will return a coll where each element at index
     n corresponds to the element found at index of n in the given coll. The value of the element at
     index n, call it k, in the returned coll states it is the kth lowest value in the given coll (elements
     of the same value can have the same ranking).
     
     i.e.
         [3 1 2] -> [2 0 1]
         [6 1 1 1 4 7] -> [2 0 0 0 1 3]"
    [coll]
    (let [ranking (let [ranks (sort (set coll))]
                    (reduce #(assoc %1 %2 (.indexOf ranks %2)) {} coll))]
      (map #(ranking %) coll)))
  
  (element-rank [6 1 1 1 4 7])

  (#(clojure.string/includes? % " ") "Heyo ")
  )