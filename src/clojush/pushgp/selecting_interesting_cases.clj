(ns clojush.pushgp.selecting-interesting-cases
  (:require [clojush.util :as util]
            [clojure.set :as cset]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn getting-input-output-pairs
  [num-of-cases sorted-indices inputs outputs]
  (for [i (range num-of-cases)
        :let [current-index (nth sorted-indices i)
              the-output-to-be-presented (nth outputs current-index)
              the-input-to-be-presenetd (nth inputs current-index)]]
    (vector the-input-to-be-presenetd the-output-to-be-presented)))

(defn sort-cases-by-trace
  [training-set-traces new-cases-traces inputs outputs num-of-cases]
  (let [bool-results (map (fn [the-new-case-traces]
                            (map (fn [the-training-case]
                                   (map = the-new-case-traces the-training-case))
                                 training-set-traces))
                          new-cases-traces)
        count-results (map (fn [bool-results-from-one-case]
                            (map (fn [bool-results-from-one-training-case]
                                   (count (filter #(identity %) bool-results-from-one-training-case)))
                                 bool-results-from-one-case))
                           bool-results)
        sorted-indices (map first (sort-by (comp #(apply min %) second) (map-indexed vector count-results)))
        sorted-diff (map second (sort-by (comp #(apply min %) second) (map-indexed vector count-results)))]
    (println (take 5 sorted-diff))
    (getting-input-output-pairs num-of-cases sorted-indices inputs outputs))
  )

(defn sort-cases-by-trace-the-second-whole
  [training-set-traces new-cases-traces]
  (let [bool-results (map (fn [the-new-case-traces]
                            (map (fn [the-training-case]
                                   (= the-new-case-traces the-training-case))
                                 training-set-traces))
                          new-cases-traces)
        count-results (map (fn [the-list]
                             (count (filter #(identity (not %)) the-list)) bool-results))]
   count-results))

(comment
  (def training-trace [['(0 0 0 0 0 0 0 0 0 1)
                        '(0 0 0 0 0 0 0 0 0 6)
                        '(1 0 0 0 0 0 0 0 0 5)
                        '(2 0 0 0 0 0 0 0 0 4)
                        '(1 0 0 0 0 0 0 0 0 3)
                        '(2 0 0 0 0 0 0 0 0 2)
                        '(3 0 0 0 0 0 0 0 0 1)]

                       ['(0 0 0 0 0 0 0 0 0 9)
                        '(3 0 0 0 0 0 0 0 0 8)
                        '(1 0 0 0 0 0 0 1 0 5)
                        '(2 0 0 2 0 0 0 0 0 4)
                        '(1 0 0 0 0 0 0 0 0 3)
                        '(2 0 0 0 0 0 0 0 3 2)
                        '(2 0 0 0 0 0 0 0 0 1)]])
  (def new-traces [['(0 0 0 0 0 0 0 0 0 1)
                    '(0 0 0 0 0 0 0 0 0 6)
                    '(1 0 0 0 0 0 0 0 0 5)
                    '(2 0 0 0 0 0 0 0 0 4)
                    '(1 0 0 0 0 0 0 0 0 3)
                    '(2 0 0 0 0 0 0 0 0 2)
                    '(3 0 0 0 0 0 0 0 0 1)]
                   
                   ['(0 0 0 0 0 0 0 0 0 2)
                    '(0 0 0 0 0 0 1 0 0 6)
                    '(1 0 0 0 0 0 0 0 0 5)
                    '(2 0 0 4 0 0 0 0 0 4)
                    '(1 0 0 0 0 0 3 0 0 3)
                    '(2 0 0 0 0 0 0 0 0 2)
                    '(3 0 0 0 4 0 0 0 0 1)]

                   ['(1 0 0 0 0 0 0 0 0 9)
                    '(3 0 0 0 0 0 0 0 0 8)
                    '(1 0 0 0 0 0 0 1 0 5)
                    '(2 0 0 2 0 0 0 0 0 4)
                    '(1 0 0 0 0 0 0 0 0 3)
                    '(2 0 0 0 0 0 0 0 3 2)
                    '(2 0 0 0 0 0 0 0 0 1)]])
  (def fake-randomly-generated-cases [[[1 4] []] [[2 3 4] []] [[1 4 3] []]])
  (def new-output-seq '([] ["-1" "2" "3"] ["-2" "-209"] ["0" "-af" "2fq"]))
  (sort-cases-by-trace-the-second-whole training-trace new-traces)
  )

(defn measure-output-difference
  "Gives a value that states the difference between the current training set 
   and the new generated input. The larger the value is, the more different two outputs are
   @param current-training-set-output the output from current training set
   @param new-output-seq the output of the randomly generated cases
   @param output-type the data type of the output, ie. [:integer], [:vectorof :string]
   @return a list that contains the difference of the two outputs at the same index. 
   int/float difference: absolute value of x - y
   string difference: Levenshtein distance, with 0 indicating identity and 1 indicating no similarity
   vectorof difference: depends on the data type inside the vector"
  [current-training-set-output new-output-seq output-type]
  (let [output-type-1 (nth output-type 0)] 
    (cond (or (= output-type-1 :integer) (= output-type-1 :float))
          (map (fn [new-output]
                 (map (fn [training-set-output]
                        (Math/abs (- training-set-output new-output))) current-training-set-output)) new-output-seq)

          (= output-type-1 :string)
          (map (fn [new-output]
                 (map (fn [training-set-output]
                        (util/levenshtein-distance training-set-output new-output)) current-training-set-output)) new-output-seq)

          :else
          (map (fn [new-output]
                 (map util/mean (map (fn [item1]
                        (let [item1-size (count item1)
                              item2-size (count new-output)
                              item1-set (set item1)
                              item2-set (set new-output)
                              size-difference (Math/abs (- item1-size item2-size))
                              result-difference (measure-output-difference item1 new-output (vector (nth output-type 1)))
                              num-of-distinct-elements (count (into (cset/difference item1-set item2-set)
                                                                    (cset/difference item2-set item1-set)))]
                          (conj (apply concat result-difference) num-of-distinct-elements size-difference)))
                      current-training-set-output)))
               new-output-seq))))

(defn get-output-types
  "Formatting the output data type keywords so that they can be passed to the output-analysis function
   @param output-keyword the keyword that states what the data type the output is
   @return a vector of output data types; if the output is :vector_someType, return [:vector :someType]
   else return [:output-keyword]"
  [output-keyword]
  (vec (map keyword (str/split (name output-keyword) #"_"))))

(defn output-analysis
  "Use the max-min function to measure how each new case is different from the original training cases
   and return the num-of-cases-to-be-presented of cases that are the most different cases
   @param training-set-output a vector of outputs from the current training set
   @param new-output-seq a vector of outputs from the randomly generated cases
   @param output-types the data types of the output
   @param num-of-cases-to-be-presented the number of cases to be returned
   @return num-of-cases-to-be-presented of cases"
  [training-set-output new-outputs new-inputs output-types num-of-cases-to-be-presented]
  (let [separated-output-types (get-output-types output-types)
        result-difference (measure-output-difference training-set-output
                                                     new-outputs
                                                     separated-output-types)
        sorted-indices (map first (sort-by (comp #(apply min %) second) > (map-indexed vector result-difference)))]
    (getting-input-output-pairs num-of-cases-to-be-presented sorted-indices new-inputs new-outputs)))

(defn get-chosen-inputs
  "Deconstructing the output-input pair to get the inputs. Return a list of input sets"
  [output-input-pairs]
  (map first output-input-pairs))

(defn get-chosen-outputs
  "Deconstructing the output-input pair to get the outputs. Return a list of output sets"
  [output-input-pairs]
  (map second output-input-pairs))

(comment
  ;; Output-analysis test
  (def training-set-output '(["1" "2"] ["4" "5" "10"]))
  (def new-output-seq '([] ["-1" "2" "3"] ["-2" "-209"] ["0" "-af" "2fq"]))
  (def fake-randomly-generated-cases [[[1 4] []] [[2 3 4] []] [[1 4 3] []] [[7 7 7] []]])
  (let [pairs (output-analysis training-set-output new-output-seq fake-randomly-generated-cases :vector_string 4)]
    (println (get-chosen-inputs pairs))
    (println (get-chosen-outputs pairs)))
   )

(defn add-edge-number-cases
  "Helper functions to added a new edge case with int/float data type
   @param cases-to-be-added the initial vector to add the case to
   @param input-range a map that contains the upper and lower bound for the int/float input
   @param input-type the type of the input: either :integer or :float
   @param input-range the difference of the upper and lower bound
   @param input-lower the lower bound of the int/float input
   @param target-keyword either :upper or :lower depending on the user
   @return a vector that contains the upper bound or the lower bound of the int/float input
           or a randomly generated int/float using the second function body"
  ([case-to-be-added input-range target-keyword]
   (conj case-to-be-added
         (get input-range target-keyword)))

  ([case-to-be-added input-type input-range input-lower]
   (conj case-to-be-added
         (if (= input-type :integer)
           (+ (rand-int input-range) input-lower)
           (+ (rand input-range) input-lower)))))

(defn add-edge-string-cases
  "Helper functions to added a new edge case with string data type
   @param cases-to-be-added the initial vector to add the case to
   @param str-len the length of the string
   @param all-possible-chars the available characters that can be used
   @return a vector that contains the randomly generated string of str-len"
  [case-to-be-added str-len all-possible-chars]
  (conj case-to-be-added
        (apply str (repeatedly str-len #(rand-nth all-possible-chars)))))

(defn formating-vectorof-input
  "Helper function for generating-edge-cases to format vectorof input
   @param num-of-elements-lower integer that states the lower bound for the size of the vector
   @param num-of-elements-upper integer that states the upper bound for the size of the vector
   @param add-cases-func a function that creates the cases
   @return a vector of vectors where each vector is either the longest/shortest vector of the vectorof input"
  [num-of-elements-lower num-of-elements-upper add-cases-func]
  (vector (vec (apply concat (repeatedly num-of-elements-lower
                                         add-cases-func)))
          (vec (apply concat (repeatedly num-of-elements-upper
                                         add-cases-func)))))

(defn generate-edge-cases
  "Generating possible edge cases based on the range of the inputs.
   @param a-training-case the information of the inputs in the training set
   @return a vector of vectors where each vector contains the extreme values of a single input"
  [a-training-case]
  (reduce (fn [result-vec input]
            (let [input-type (get input :type)
                  input-range (get input :range)]
              (cond (or (= input-type :integer) (= input-type :float))
                    (let [edge-case-1 (add-edge-number-cases [] input-range :lower)]
                      (conj result-vec
                            (add-edge-number-cases edge-case-1 input-range :upper)))

                    (= input-type :string)
                    (let [all-possible-chars (get input-range :available-characters)
                          lower (get input-range :lower)
                          upper (get input-range :upper)
                          edge-case-1 (add-edge-string-cases [] lower all-possible-chars)]
                      (conj result-vec
                            (add-edge-string-cases edge-case-1 upper all-possible-chars)))

                    :else
                    (let [smallest-vector (get input-range :lower)
                          largest-vector (get input-range :upper)
                          element-type (get input :element-type)
                          element-upper (get-in input [:element-range :upper])
                          element-lower (get-in input [:element-range :lower])
                          element-range (- element-upper element-lower)
                          element-characters (get-in input [:element-range :available-characters])]
                      (if (= element-type :string)
                        (conj result-vec
                              (formating-vectorof-input smallest-vector
                                                        largest-vector
                                                        #(add-edge-string-cases []
                                                                                (+ (rand-int element-range) element-lower)
                                                                                element-characters)))
                        (conj result-vec
                              (formating-vectorof-input smallest-vector
                                                        largest-vector
                                                        #(add-edge-number-cases []
                                                                                element-type
                                                                                element-range
                                                                                element-lower))))))))
          []
          a-training-case))

(defn swap-it
  "Swap the values in the first vector with the values in the second vector at corresponding indices
   @param edge-1 the template vector 
   @param edge-2 the vector that contains all potential values to be swaped
   @param swaps the indices to be swaped"
  [edge-1 edge-2 swaps]
  (map (fn [index]
         (if (some #{index} swaps)
           (nth edge-2 index)
           (nth edge-1 index)))
       (range (count edge-1))))

(defn forming-input-output-sets
  "Return all the combinations of edge cases and format them so that they have fake outputs
     @param vector-of-inputs a vector of two edge cases
     @return a list of vectors where each vector contains a input-output pair(fake output is [])"
  [vector-of-inputs]
  (map #(vector % [])
       (let [edge (apply mapv
                         vector
                         (generate-edge-cases vector-of-inputs))
             edge-1 (get edge 0)
             edge-2 (get edge 1)
             cols (count (first edge))
             subsets (combo/subsets (range cols))]
         (map #(swap-it edge-1 edge-2 %) subsets))))

(comment
  ;; edge-cases test
  (def training-set [{:type :integer
                      :range {:lower 0
                              :upper 10}}

                     {:type :string
                      :range {:lower 1
                              :upper 5
                              :available-characters "01234"}}

                     {:type :float
                      :range {:lower 1.001
                              :upper 10.999}}])
  (forming-input-output-sets training-set))

