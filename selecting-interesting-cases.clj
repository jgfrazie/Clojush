(ns summer-clojure.core
  (:require [clojush.util :as util]))

(use 'clojure.set)

;;;;;;;;;;;;;;;
;; This is preparing for examine the outputs
;; first we have to run the best program to get the outputs and store them 
;; -> (we can also get this during the GP run so that we don't have a secondary GP run)
(defn run-best-program
  "Returns the output of the target training case from the best program"
  [best-program training-case output-type]
  (let [state (->> (make-push-state)
                   (map #(push-item (get % :param) :input)
                        training-case)
                   (run-push (:program best-program)))]
    (top-item output-type state)))

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
  (let [output-type-1 (get output-type 0)]
   (cond (or (= output-type-1 :integer) (= output-type-1 :float))
        (map (fn [training-set-output new-output]
               (Math/abs (- training-set-output new-output))) current-training-set-output new-output-seq)

        (= output-type-1 :string)
        (map (fn [output-1 output-2]
              (- 1 (util/sequence-similarity output-1 output-2))) current-training-set-output new-output-seq)

        :else
        (map util/mean (map (fn [item1 item2]
                                (let [item1-size (count item1)
                                      item2-size (count item2)
                                      item1-set (set item1)
                                      item2-set (set item2)
                                      size-difference (Math/abs (- item1-size item2-size))
                                      result-difference (measure-output-difference item1 item2 (vector (get output-type 1)))
                                      num-of-distinct-elements (count (into (difference item1-set item2-set)
                                                                            (difference item2-set item1-set)))]
                                  (conj result-difference num-of-distinct-elements size-difference)))
                              current-training-set-output
                              new-output-seq)))))



(comment
  (def training-set-output [["1" "3"] ["2" "3" "4" "10"]])
  (def new-output-seq [["1" "4"] ["2" "4" "3"]])
  (def new-t (shuffle training-set-output))
  (def new-new-o (shuffle new-output-seq))
  (into new-output-seq training-set-output)
  (measure-output-difference training-set-output new-output-seq [:vectorof :string])
  (measure-output-difference new-t new-new-o [:vectorof :string])
  (def inputss [[{:type "int"
                  :param 0}
                 {:type "string"
                  :param "10"}]
                [{:type "int"
                  :param 40}
                 {:type "string"
                  :param "100"}]])
  ;; a lazy seq of the outputs corresponding to the lazy seq of random inputs(using the best program)
  (let [output ((fn get-output
                  [i]
                  (lazy-seq (cons (run-best-program best-program (get inputss i) output-type) (get-output (inc i)))))
                0)])

  (count (distinct (into (get training-set-output 0) (get new-output-seq 0))))
  (map (fn [item1 item2]
         (count (distinct (into item1 item2)))) training-set-output new-output-seq)

  (- 1 (util/sequence-similarity "0" "1"))
  (map + [1 2] [1 1])
  (map (fn [item1 item2]
         (map + item1 item2)) training-set-output new-output-seq)
  (integer? "d")
  ((map (fn [item1 item2]
          (map (fn [i1 i2]
                 (conj lala (vector i1 i2))) item1 item2)) new-output-seq training-set-output))
  (vector? "1")
  (into (vector 1 2) [4 4])
  )


(defn add-edge-number-cases
  "Helper functions to added a new edge case that of int/float data type"
  ([case-to-be-added input-range target-keyword]
   (conj case-to-be-added
         (get input-range target-keyword)))

  ([case-to-be-added input-type element-range element-lower]
   (conj case-to-be-added
         (if (= input-type :integer)
                   (+ (rand-int element-range) element-lower)
                   (+ (rand element-range) element-lower)))))


(defn add-edge-string-cases
  "Helper functions to added a new edge case that of string data type"
  [case-to-be-added str-len all-possible-chars]
  (conj case-to-be-added
        (apply str (repeatedly str-len #(rand-nth all-possible-chars)))))


(defn generate-edge-cases
  "Generating possible edge cases based on the range of the inputs.
   @param a-training-case one case from the training set
   @return a vector of two edge cases for the training set"
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
                              (vector (vec (apply concat (repeatedly smallest-vector
                                                                     #(add-edge-string-cases []

                                                                                             (+ (rand-int element-range) element-lower)
                                                                                             element-characters))))
                                      (vec (apply concat (repeatedly largest-vector
                                                                     #(add-edge-string-cases []

                                                                                             (+ (rand-int element-range) element-lower)
                                                                                             element-characters))))))
                        (conj result-vec
                              (vector (vec (apply concat (repeatedly smallest-vector
                                                                     #(add-edge-number-cases []
                                                                                             element-type
                                                                                             element-range
                                                                                             element-lower))))
                                      (vec (apply concat (repeatedly largest-vector
                                                                     #(add-edge-number-cases []
                                                                                             element-type
                                                                                             element-range
                                                                                             element-lower)))))))))))
          []
          a-training-case))

  (comment
  ;; random tests
    (def training-set [{:type :integer
                        :range {:lower 0
                                :upper 10}
                        :param 0}
                       
                       {:type :string
                        :range {:lower 1
                                :upper 5
                                :available-characters "01234"}
                        :param "10"}
                       
                       {:type :float
                        :range {:lower 1.001
                                :upper 10.999}
                        :param 5.5}
                       
                       {:type :vectorof
                        :range {:lower 2 :upper 3}
                        :element-type :string
                        :element-range {:lower 0 
                                        :upper 12 
                                        :available-characters "-0 abcdefghijklmnopqrstuvwxyz !@#$%^&*()_+-=`~,<.>/?]}[{"}}
                       
                       {:type :vectorof
                        :range {:lower 2 :upper 3}
                        :element-type :integer
                        :element-range {:lower 0
                                        :upper 12}}])
    (map shuffle (generate-edge-cases training-set))
    )

