(ns clojush.pushgp.counterexample-driven-gp
  (:use [clojush random args pushstate interpreter globals individual util])
  (:require [clojush.pushgp.selecting-interesting-cases :as interesting])
  (:require [clojush.pushgp.case-auto-generation :as cag]))

; NOTE: When using counterexample-driven GP, only uses the current set of training
;       cases when simplifying at the end of a run. While intentional for now,
;       we may decide later that it would be preferable to use all available cases
;       for this simplification, which may result in better simplification.

(defn counterexample-check-results-automatic
  "Checks if the best program passed all generated cases, returning true
  if so and false otherwise. If not, also updates :sub-training-cases by
  adding a wrong case.
  This version is automatic, using the known right answers."
  [all-cases best-results-on-all-cases {:keys [output-stacks
                                               counterexample-driven-number-cases-to-add]}]
  (let [wrong-cases (remove #(= % :right-answer-on-case)
                            (map (fn [case best-result]
                                   (if (if (= output-stacks :float)
                                         ; NOTE: this won't work for Number IO and other problems with printed float outputs
                                         (= (round-to-n-decimal-places (last case) 4)
                                            (round-to-n-decimal-places best-result 4))
                                         (= (last case) best-result))
                                     :right-answer-on-case
                                     case))
                                 all-cases
                                 best-results-on-all-cases))]
    (if (empty? wrong-cases)
      :passes-all-cases ; program passes all generated cases
      (let [wrong-cases-besides-those-already-added (remove #(some #{%} (:sub-training-cases @push-argmap))
                                                            wrong-cases)]
        (if (empty? wrong-cases-besides-those-already-added)
          :passes-all-cases-besides-those-in-sub-training-cases
          (let [counterexample-cases-to-add (take counterexample-driven-number-cases-to-add
                                                 (lshuffle wrong-cases-besides-those-already-added))]
            ; add case to sub-training-cases
            (println "Adding case(s) to sub-training-cases:" (pr-str counterexample-cases-to-add))
            ;(print "Press enter to continue...") (flush) (read-line)
            counterexample-cases-to-add)))))) ; return counterexample since program does not pass all generated cases

(defn finish-adding-cases-to-training-set
  "Takes a list of the wrong cases and merges it with a list of
   the corresponding right answers from the user"
  [counterexample-cases-to-add wrong-cases output-types]
  (vec (map vector (loop [inputs []
                          index 0]
                     (if (< index (count counterexample-cases-to-add))
                       (recur (conj inputs (first (nth counterexample-cases-to-add index)))
                              (inc index))
                       inputs))
            (loop [index 0
                   right-answers []]
              (if (< index (count wrong-cases))
                (do (println "What is the right answer for case" (nth wrong-cases index) 
                             "with input" (first (nth counterexample-cases-to-add index)) 
                             "? Separate by spaces if it's a vector!" output-types)
                    (recur (inc index) (apply conj right-answers (loop [outputs []
                                                                        index 0]
                                                                   (if (< index (count output-types))
                                                                     (recur (conj outputs
                                                                                  (cond
                                                                                    (= (nth output-types index) :integer) (vec (map #(Integer/parseInt %) (clojure.string/split (read-line) #" ")))
                                                                                    (= (nth output-types index) :float) (vec (map #(Float/parseFloat %) (clojure.string/split (read-line) #" ")))
                                                                                    (= (nth output-types index) :string) (clojure.string/split (read-line) #" ")
                                                                                    (= (nth output-types index) :boolean) (do (println "Type in 0 for false or 1 for true:")
                                                                                                                              (vec (loop [zeros-and-onex (map #(Integer/parseInt %) (clojure.string/split (read-line) #" "))
                                                                                                                                          boolean-outputs []
                                                                                                                                          index 0]
                                                                                                                                     (if (< index (count zeros-and-onex))
                                                                                                                                       (if (= (nth zeros-and-onex index) 0)
                                                                                                                                         (recur zeros-and-onex (conj boolean-outputs false) (inc index))
                                                                                                                                         (recur zeros-and-onex (conj boolean-outputs true) (inc index)))
                                                                                                                                       boolean-outputs))))))
                                                                            (inc index))
                                                                     outputs)))))
                right-answers))))) 

(defn counterexample-check-results-human
  "Checks if the best program passed all generated cases, returning true
  if so and false otherwise. If not, also updates :sub-training-cases by
  adding a wrong case.
  This version uses human interaction to determine if any cases are wrong
  and pick a wrong one if so.

  NOTE WHEN IMPLEMENTING: Should print all case inputs and best outputs,
  numbered, and have user enter the number of a wrong case or correct if
  they are all correct."
  [random-cases best-results-on-all-cases output-types]
  (doseq [[i x] (map-indexed vector
                             (map vector random-cases best-results-on-all-cases))]
    (println "Case" i ": Generated random input: " (pr-str (first (first x))) "; Output from best program:" (pr-str (second x)) "; Case" i))
  (prn "Are all these correct? Y for Yes, N for No, any other character to continue evolving: ")
  (let [answer (read-line)] answer
       (cond
         (or (= "Y" answer) (= "y" answer)) :passes-all-cases ; program passes all randomly generated cases
         (or (= "N" answer) (= "n" answer)) (do (prn "Which cases are wrong? Enter the numbers separated by a space: ")
                            (flush)
                            (let [str-wrong (read-line)]
                              (let [wrong-cases (filter #(and (< % (count random-cases)) (>= % 0))
                                                        (vec (map #(Integer/parseInt %)
                                                                  (re-seq #"\d+" str-wrong))))]
                                (let [counterexample-cases-to-add (for [case-num wrong-cases]
                                                                    (nth random-cases case-num))]
                                  
                                  (finish-adding-cases-to-training-set counterexample-cases-to-add wrong-cases output-types))))))))

(defn counterexample-check-results-simulated-human
  "Simulated human interaction to decide whether the best program 
   passes all taining cases or not. It runs an oracle function of the worng cases
   to get the right answer as would the user do. Finally, it returns the pairs of 
   incput and its corresponding correct output."
  [random-cases best-results-on-all-cases oracle-function]

  (println)
  (println "*** The foolowing are the randomly generated test cases along  ***")
  (println "*** with its corresponding output from the best program we have so far! ***")
  (println)

  (doseq [[i x] (map-indexed vector
                             (map vector random-cases best-results-on-all-cases))]
    (println "Case" i ": Generated random input: " (pr-str (first (first x))) ", Output from best program:" (pr-str (second x)) "; Case" i))
  (println)
  (loop [input random-cases
         output best-results-on-all-cases
         cases-to-add []
         index 0]
    ;(println)
    ;(pr-str "I am here!!!!!!!!!!!!!!!!!!!!!!" (first (nth input 0)))
    ;(pr-str "I am here!!!!!!!!!!!!!!!!!!!!!!" (first (nth input 1)))
    ;(pr-str "I am here!!!!!!!!!!!!!!!!!!!!!!" (first (nth input 3)))
    ;(pr-str "I am here!!!!!!!!!!!!!!!!!!!!!!" (first (nth input 4)))
    ;(println)
    (if (< index (count random-cases))
      (let [right-answer (apply oracle-function (first (nth random-cases index)))]
        ;(println)
        ;(println "The right answer is: " right-answer)
        ;(println "The nth output is: " (nth output index))
        ;(println)
        (if (= (vector right-answer) (nth output index))
          (recur random-cases best-results-on-all-cases cases-to-add (inc index))
          (recur random-cases best-results-on-all-cases
                 (into cases-to-add
                       (vec (map vector (vector (first (nth input index))) (vector (vector right-answer)))))
                 (inc index))))
      (do (println "Number of cases to add are: " (count cases-to-add))
          (if (= (count cases-to-add) 0)
            :passes-all-cases ; program passes all randomly generated cases
            cases-to-add)))))

(defn proportion-of-passed-cases
  "Returns the proportion of cases with 0 error for this individual."
  [ind]
  (let [errors (:errors ind)
        num-zero-errors (count (filter zero? errors))]
    (/ num-zero-errors (count errors))))

(defn generate-counterexample-type
  "Will be mapped over all counterexample types to generate cases."
  [counterexample-type best
   {:keys [training-cases input-parameterization
           num-of-cases-added-from-random max-num-of-cases-added-from-edge input-constrains]
    :as argmap}]
  (case counterexample-type
    :hard-coded training-cases
    :randomly-generated (interesting/check-for-input-constraints input-constrains (cag/generate-random-cases input-parameterization num-of-cases-added-from-random))
    :edge-cases (interesting/check-for-input-constraints input-constrains (interesting/forming-input-output-sets input-parameterization max-num-of-cases-added-from-edge))
    :selecting-new-cases-based-on-outputs (interesting/check-for-input-constraints input-constrains (interesting/choose-inputs-based-on-output-analysis best argmap))
    :branch-coverage-test (interesting/check-for-input-constraints input-constrains (interesting/sort-cases-by-trace-the-second-whole best argmap))
    :else (throw (str "Unrecognized option for :counterexample-driven-case-generators: "
                      counterexample-type))))

(defn check-if-all-correct-and-return-new-cases-if-not
  "Finds the best program's behavior on all generated cases and checks if all outputs
  are correct with the given case checker.
  Returns solution individual if there is one.
  Returns set of new counterexample cases if not a solution."
  [sorted-pop {:keys [counterexample-driven-case-generators counterexample-driven-case-checker
                      error-threshold counterexample-driven-fitness-threshold-for-new-case
                      output-stacks oracle-function] :as argmap}]
  (println)
  (println "*** A program was found that passes all of the training cases! ***")
  (println "*** Now it's time to check if the best program works on some new inputs: ***")
  (println)
  (let [best (first sorted-pop)
        all-cases (apply concat
                         (map #(generate-counterexample-type % best argmap)
                              counterexample-driven-case-generators))]
    (loop [best (first sorted-pop)
           pop (rest sorted-pop)
           new-cases '()]
      ;; (println "HERE'S THE BEST PROGRAM:" best)
      (let [best-results-on-all-cases (map first (interesting/run-best-on-all-cases best all-cases argmap))
            counterexample-cases (case counterexample-driven-case-checker
                                   :automatic (counterexample-check-results-automatic
                                               all-cases best-results-on-all-cases argmap)
                                   :human (counterexample-check-results-human
                                           all-cases best-results-on-all-cases output-stacks)
                                   :simulated-human (counterexample-check-results-simulated-human
                                                     all-cases best-results-on-all-cases oracle-function))
            new-cases-with-new-case (if (keyword? counterexample-cases)
                                      new-cases
                                      (concat counterexample-cases new-cases))]
        (when (and (seq? counterexample-cases)
                   (some (set counterexample-cases) (:sub-training-cases @push-argmap)))
          (println "Houston, we have a problem. This case is already in the training cases, and has been passed by this program.")
          (prn "existing cases: " (:sub-training-cases @push-argmap))
          (prn "new case(s): " counterexample-cases)
          (prn "best individual: " best)
          (prn "run it on new case:" (first (map first (interesting/run-best-on-all-cases best counterexample-cases argmap))))
          (throw (Exception. "Added a new case already in training cases. See above.")))
        (cond
          ; Found a solution, return it
          (= counterexample-cases :passes-all-cases)
          best
          ; Didn't find a solution; if rest of population is empty, return new-cases
          (empty? pop)
          new-cases-with-new-case
          ; If there's more pop, see if next program also has 0 on training error.
          ; If so, recur
          (and (= counterexample-driven-case-checker :automatic)
               (if (>= counterexample-driven-fitness-threshold-for-new-case 1.0)
                 (<= (:total-error (first pop)) error-threshold)
                 (> (proportion-of-passed-cases (first pop))
                    counterexample-driven-fitness-threshold-for-new-case)))
          (recur (first pop)
                 (rest pop)
                 new-cases-with-new-case)
          ; If here, no more individuals with 0 training error, so return false
          :else
          new-cases-with-new-case)))))

(def generations-since-last-case-addition (atom -1))

(defn add-cases-to-sub-training-cases
  "Adds new-cases to existing sub-training-cases. Also updates
  atom signifying number of generations since last addition.
  Returns nil."
  [population new-cases {:keys [counterexample-max-cases-before-removing-easiest
                                error-threshold] :as current-argmap}]
  (let [distinct-new-cases (distinct new-cases)
        old-cases (:sub-training-cases current-argmap)
        need-case-removals (> (+ (count distinct-new-cases)
                                 (count old-cases))
                              counterexample-max-cases-before-removing-easiest)
        case-pass-counts-map (if need-case-removals
                               (zipmap
                                old-cases
                                (map (fn [case-index]
                                       (count
                                        (filter #(<= (nth (:errors %) case-index)
                                                     error-threshold)
                                                population)))
                                     (range (count old-cases))))
                               {})
        old-cases-with-removals (if need-case-removals
                                  (loop [old-cases-map case-pass-counts-map]
                                    (cond
                                      ; We have few enough cases
                                      (<= (+ (count distinct-new-cases)
                                             (count old-cases-map))
                                          counterexample-max-cases-before-removing-easiest)
                                      (keys old-cases-map)

                                      ; Removed all old-cases. In this case, just return empty list
                                      (empty? old-cases-map)
                                      '()

                                      ; Recur, dissocing the key with most passes
                                      :else
                                      (recur (dissoc old-cases-map
                                                     (first
                                                      (apply max-key
                                                             val
                                                             old-cases-map))))))
                                  old-cases)]
    (reset! generations-since-last-case-addition 0)
    (swap! push-argmap (fn [current-argmap] ; if cases, concat them to old cases
                         (assoc current-argmap
                                :sub-training-cases
                                (take counterexample-max-cases-before-removing-easiest ; want at most this many cases
                                      (concat distinct-new-cases
                                              old-cases-with-removals)))))
    nil))

(defn generational-case-addition
  "Adds one case that best program doesn't pass to sub-training-cases.
  Returns nil."
  [best population {:keys [counterexample-driven-case-generators training-cases
                counterexample-driven-case-checker] :as argmap}]
  (let [all-cases (case counterexample-driven-case-generators
                    :hard-coded training-cases
                    :else (throw (str "Unrecognized option for :counterexample-driven-case-generators: "
                                      counterexample-driven-case-generators)))
        best-results-on-all-cases (map first (interesting/run-best-on-all-cases best all-cases argmap))
        counterexample-cases (case counterexample-driven-case-checker
                              :automatic (counterexample-check-results-automatic
                                          all-cases best-results-on-all-cases argmap)
                              :human (counterexample-check-results-human
                                      all-cases best-results-on-all-cases))]
    (cond
      ; This shouldn't happen, but could
      (= counterexample-cases :passes-all-cases)
      nil
      ; This could happen if the sub-training cases gets large. If so, just ignore
      ; and add another case next generation.
      (= counterexample-cases :passes-all-cases-besides-those-in-sub-training-cases)
      nil
      ; This could happen. If so, just ignore it and add another next generation
      (and (seq? counterexample-cases)
           (some (set counterexample-cases) (:sub-training-cases @push-argmap)))
      nil
      ; Add the case to training cases
      :else
      (add-cases-to-sub-training-cases population counterexample-cases argmap))))

(defn check-counterexample-driven-results
  "Returns true if a program has been found that passes all generated training
  cases, and false otherwise.

  Needs to:
  - check if any programs pass all training cases
   - if so, generate new cases and have checker check if any program passes all of those
    - if so, return that program as a success
    - else, have checker pick case that has wrong answer, have checker give right answer, and add that case to sub-training-cases. return false
   - else, return false

  If not individuals pass all cases, check if need to add a generational case."
  [sorted-pop {:keys [error-threshold counterexample-driven-add-case-every-X-generations
                      counterexample-driven-fitness-threshold-for-new-case] :as argmap}]
  
  (swap! generations-since-last-case-addition inc)
  (if (if (>= counterexample-driven-fitness-threshold-for-new-case 1.0)
        (> (:total-error (first sorted-pop)) error-threshold)
        (< (proportion-of-passed-cases (first sorted-pop))
           counterexample-driven-fitness-threshold-for-new-case))
    ; This handles best individuals that don't pass all current tests
    (do
      (when (<= 1 counterexample-driven-add-case-every-X-generations @generations-since-last-case-addition)
        (generational-case-addition (first sorted-pop) sorted-pop argmap))
      false)
    ; This handles best individuals that pass all current cases
    (let [best-or-new-cases (check-if-all-correct-and-return-new-cases-if-not sorted-pop argmap)]
      (if (= (type best-or-new-cases) clojush.individual.individual)
        best-or-new-cases ; if an individual, it is a success, so return it
        ; Otherwise, add in the new cases, and return false.
        (do
          (add-cases-to-sub-training-cases sorted-pop best-or-new-cases argmap)
          false)))))

(count [false false true])

