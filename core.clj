(ns summer-clojure.core
  (:require [clojush.util :as util]))

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

(defn get-unique-element
  "If the element is in both sequences, don't add it to result list.
   If the element is in only one of the sequence, add it to result list."
  [seq1 seq2]
  (filter (fn [element]
            (not (some #(= element %) seq1)))
          seq2))

(defn measure-output-difference
  "Gives a value that states the difference between the current training set 
   and the new generated input. The larger the value is, the more different the two outputs are."
  [all-training-set-output new-output-seq output-type]
  (cond (or (= output-type :int) (= output-type :float))
        (map (fn [training-set-output new-output]
               (Math/abs (- training-set-output new-output))) all-training-set-output new-output-seq)

        (= output-type :string)
        (map util/sequence-similarity all-training-set-output new-output-seq)

        :else
        (map util/mean (map (fn [item1 item2]
                                (let [element-type (get output-type 1)
                                      item1-size (count item1)
                                      item2-size (count item2)
                                      num-of-distinct-outputs (let [dist-list (distinct (into item1 item2))]
                                                               (count (if (> item1-size item2-size)
                                                                 (get-unique-element item2 dist-list)
                                                                 (get-unique-element item1 dist-list))))
                                      result-difference (measure-output-difference item1 item2 element-type)
                                      items-size-difference (Math/abs (- item1-size item2-size))]
                                  (conj result-difference items-size-difference num-of-distinct-outputs)))
                              all-training-set-output
                              new-output-seq))))


(comment
  (into [1 2 3] [1 2])
  (def training-set-output [["1" "0" "0" "3"] ["3" "4" "1" "0"]])
  (def new-output-seq [["1" "3" "2" "0"] ["4" "3" "1"]])
  (measure-output-difference training-set-output new-output-seq [:vectorof :string])
  (def item2 [1 2])
  (def dist-list [1 2 4])
  (some #(not (= 1 %)) item2)
  (filter (fn [element]
            (not (some #(= element %) item2)))
          dist-list)

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

  (map util/sequence-similarity ["10" "20"] ["1" "1"])
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
  [case-to-be-added input-type input-range target-keyword]
  (conj case-to-be-added
        {:type input-type
         :range input-range
         :param (get input-range target-keyword)}))

(defn add-edge-string-cases
  "Helper functions to added a new edge case that of string data type"
  [case-to-be-added input-type input-range str-len all-possible-chars]
  (conj case-to-be-added
        {:type input-type
         :range input-range
         :param (apply str (repeatedly str-len #(rand-nth all-possible-chars)))}))

(defn generate-edge-cases
  "Generating possible edge cases based on the range of the inputs."
  [a-training-case]
  (let [input-types (map #(get % :type) a-training-case)
        input-ranges (map #(get % :range) a-training-case)
        num-of-params (count a-training-case)]
    ;; loop through all the parameters
    (loop [i 0
           input-type (get input-types i)
           input-range (get input-ranges i)
           edge-case-1 []
           edge-case-2 []]
      (if (= i num-of-params)
        (edge-case-1 edge-case-2)
        (cond (or (= input-type :int) (= input-type :float))
              (do (add-edge-number-cases edge-case-1 input-type input-range :lower)
                  (add-edge-number-cases edge-case-2 input-type input-range :upper))

              (= input-type :string)
              (let [all-possible-chars (get input-ranges :available-characters)
                    lower (get input-range :lower)
                    upper (get input-range :upper)]
                (add-edge-string-cases edge-case-1 input-type input-range lower all-possible-chars)
                (add-edge-string-cases edge-case-2 input-type input-range upper all-possible-chars))

              (= input-type :vectorof)
              (let [smallest-vector (get input-range :lower)
                    largest-vector (get input-range :upper)
                    element-type (get a-training-case :element-type)
                    element-upper (get-in a-training-case [:element-range :upper])
                    element-lower (get-in a-training-case [:element-range :lower])
                    element-characters (get-in a-training-case [:element-range :available-characters])]
                (if (= element-type :string)
                  (do (vector (repeatedly smallest-vector #(add-edge-string-cases edge-case-1 input-type input-range element-lower element-characters)))
                      (vector (repeatedly largest-vector #(add-edge-string-cases edge-case-2 input-type input-range element-upper element-characters))))
                  (do (vector (repeatedly smallest-vector #(add-edge-number-cases edge-case-1 input-type input-range :lower)))
                      (vector (repeatedly largest-vector #(add-edge-number-cases edge-case-2 input-type input-range :upper))))))

              :else (recur (inc i)
                           (get input-types i)
                           (get input-ranges i)
                           edge-case-1
                           edge-case-2))))))

      (comment
  ;; random tests
        (def training-set [[{:type "int"
                             :param 0}
                            {:type "string"
                             :param "10"}]
                           [{:type "int"
                             :param 3}
                            {:type "string"
                             :param "[p]"}]])
        (def best-program "something")

        (def output-type "int")
        (conj [] {:type "boolean"
                  :range ["true" "false"]
                  :param "true"})
        (let [grade 85]
          (cond
            (>= grade 90) "A"
            (>= grade 80) (do (println "B") (println "???"))
            (>= grade 70) "C"
            (>= grade 60) "D"
            :else "F"))
        (apply str (repeatedly 10 #(rand-nth "abcdefghijklmnopqrstuvwxyz0123456789")))
        (add-edge-string-cases [] :string {:lower 10 :upper 30 :available-characters "asdfghjkl"} 10 "asdfghjkl")
        (vector (repeatedly 5 #(add-edge-string-cases [] :string {:lower 10 :upper 30 :available-characters "asdfghjkleqiozcmnv"} 10 "asdfghjkl")))
  ;; sadness
        (map (fn [item]
               (map #(get % :param) item)) training-set)

        (Math/abs -1))

