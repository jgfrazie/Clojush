;;  File: case_auto_generation.clj
;;  Desc: Controls Input/Output Parameterization, Human-Driven Atom
;;        Generator Construction, and gathering Initial Training Cases while
;;        protecting the human from making erroneous actions.
;;  Main-Functions: acquire-atom-generator-push-stacks
;;                  acquire-input-instructions
;;                  acquire-outputs-from-user
;;                  acquire-parameters-from-user
;;                  create-parameter
;;                  generate-random-case-input
;;                  generate-parameter
;;                  get-initial-training-cases-from-user
;;  Notes: - Many functions have either dead parameters in the case of future needs.
;;         - Some helper functions are used in the process of simulating human interaction
;;           for the sake of efficiency.
;;  Author: James Frazier
;;  Author's Email: james.frazier8093@gmail.com
;;  Date-Last-Edited: June 29, 2022

(ns clojush.pushgp.case-auto-generation
  (:require [clojure.math.numeric-tower])
  (:require [clojure.string])
  (:require [clojush.globals]))

(declare generate-parameter)
(declare create-parameter-from-user)

;;-------------------------------------------------------------------------------------;;

;; NOTE: The following functions are helper functions that are used throughout the file.

(defn vector-of-number-difference
  "[AUTHOR: PROF. HELMUTH]
   Implements distances between vectors of numbers. Alg:
   Add the difference in length between the program's output vector and the
   correct vector times 1000 to the absolute difference between each integer
   and the corresponding integer in the correct vector."
  [vecA vecB]
  (+ (* 1000 (clojure.math.numeric-tower/abs (- (count vecA) (count vecB))))
     (apply +
            (map #(clojure.math.numeric-tower/abs (- %1 %2))
                 vecA
                 vecB))))

(defn get-output-types
  "[AUTHOR: VIOLET SHI]
   ormatting the output data type keywords so that they can be passed to the output-analysis function
   @param output-keyword the keyword that states what the data type the output is
   @return a vector of output data types; if the output is :vector_someType, return [:vector :someType]
   else return [:output-keyword]"
  [output-keyword]
  (vec (map (fn [split-word]
              (if (= "vector" split-word)
                (#(keyword %) (str split-word "of"))
                (#(keyword %) split-word))) (clojure.string/split (name output-keyword) #"_"))))

(defn add-groups-to-str
  "[HELPER FUNCTION]
   Will add a predefined group of chars to a string
   @param string A string given to concatenate to
   @param groups A vector of the groups of characters to add with the possible keywords:
          :lower-case - Adds all lower-case letters
          :upper-case - Adds all upper-case letters
          :digits - Adds all digits
          :specials - Adds a predetermined list of special characters
   @return A string with @string "
  [string groups]
  (reduce #(case %2
             :lower-case (str %1 "abcdefghijklmnopqrstuvwxyz")
             :upper-case (str %1 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
             :specials   (str %1 " !@#$%^&*()_+-=`~,<.>/?]}[{")
             :digits     (str %1 "0123456789")
             %1)
          string
          groups))

(defn process-user-input
  "[HELPER FUNCTION]
   Displays prompt for the user and will convert input to a specified data type
   @param prompt A string to be printed to terminal for the user to read
   @param type A keyword which will identify which data type is anticipated to be received.
              Options include:
                    :integer
                    :float
                    :string
   @return The user input as the specified data type"
  [prompt type]
  (println prompt)
  (case type
    :integer (Integer/parseInt (read-line))
    :string  (read-line)
    :float   (Float/parseFloat (read-line))))

(defn get-char-sets
  "[HELPER FUNCTION]
   Acquires character grouping selections from the user.
   @return A sequence of key words (possibly empty) which the user requested"
  []
  (println "Of the following options, select the corresponding number associated with
the grouping of characters followed by a space if you wish to add more than one group.
If you wish to select none of the following options, select 0.
    (1) Lower-case
    (2) Upper-case
    (3) Digits
    (4) Special Characeters")
  (let [choices (clojure.string/split (read-line) #" ")]
    (for [choice choices]
      (case choice
        "0" nil
        "1" :lower-case
        "2" :upper-case
        "3" :digits
        "4" :specials))))

(defn get-extra-chars
  "[HELPER FUNCTION]
   Acquires extra possible characters from the user which could
   occur within a given string parameter.
   @return A sequence of characters which the user inputed"
  []
  (println "List any other character options to include in this string parameter followed by a space.
If you wish to add the \" \" character, enter \"space\" to do so.
If there are no extra characters, press enter.")
  (let [extras (clojure.string/split (read-line) #" ")]
    (for [character extras]
      (if (= character "space")
        " "
        character))))

;;-------------------------------------------------------------------------------------;;

;; NOTE: The following functions are responsible for gathering Input Parameters.

(defn create-new-integer-param
  "Creates a new integer parameter type with a range specified by the user
   @param1 lower An integer of the lower bound of the range of the parameter
   @param1 upper An integer of the upper bound of the range of the parameter
   @return A map of the following format: {:type :integer, :range {:lower lower,
                                                                   :upper upper}}"
  ([]
   (println "For an Integer, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of integer range:" :integer)
         upper (process-user-input "Upper-bound of integer range:" :integer)]
     (if (> lower upper)
       (do (println "
    INVALID INPUT DETECTED. MAKE SURE LOWER-BOUND IS LESS THAN OR EQUAL TO
                               UPPER-BOUND
                     ")
           (create-new-integer-param))
       (create-new-integer-param lower upper))))

  ([lower upper]
   {:type :integer
    :range {:lower lower
            :upper upper}}))

(defn create-new-float-param
  "Creates a new float parameter type with a range specified by the user
   @param1 lower A float of the lower bound of the range of the parameter
   @param1 upper A float of the upper bound of the range of the parameter
   @return A map of the following format: {:type :float, :range {:lower, lower,
           :upper upper}}"
  ([]
   (println "For a Float, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of float range:" :float)
         upper (process-user-input "Upper-bound of float range:" :float)]
     (if (> lower upper)
       (do (println "
    INVALID INPUT DETECTED. MAKE SURE LOWER-BOUND IS LESS THAN OR EQUAL TO
                               UPPER-BOUND
                     ")
           (create-new-float-param))
       (create-new-float-param lower upper))))

  ([lower upper]
   {:type :float
    :range {:lower lower
            :upper upper}}))

(defn create-new-string-param
  "Creates a new string parameter type with a length range specified by the user
   @param lower An integer of the lower-bound of the string length
   @param upper An integer of the upper-bound of the string length
   @param char-groups A vector of the following possible key-words to add to the possible string combination:
          :lower-case
          :upper-case
          :digits
          :specials
   @param1 char-groups A string of the possible characters for each character of the string param
   @param unique-chars A listing of all the other characters to be added to the
                         possible char creation of the string that were not otherwise specified
   @return A map of the following format: {:type :string, :range {:length {:lower lower,
           :upper upper}, :available-characters #1}}"
  ([]
   (println "For a String, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of string length: " :integer)
         upper (process-user-input "Upper-bound of string length: " :integer)]
     (if (and (>= lower 0) (>= upper lower))
       (let [char-sets (get-char-sets)
             extras (get-extra-chars)]
         (create-new-string-param lower upper char-sets extras))
       (do (println "
    INVALID INPUT DETECTED. MAKE SURE LOWER-BOUND IS GREATER THAN OR EQUAL TO
       ZERO (0) AND UPPER-BOUND IS GREATER THAN OR EQUAL TO LOWER-BOUND
                     ")
           (create-new-string-param)))))

  ([lower upper char-groups unique-chars]
   (if (string? char-groups)
     (let [available-chars char-groups]
       {:type :string
        :range {:lower lower :upper upper
                :available-characters available-chars}})
     (let [available-chars (add-groups-to-str (reduce str unique-chars) char-groups)]
       {:type :string
        :range {:lower lower :upper upper
                :available-characters available-chars}}))))

(defn create-new-vectorof-param
  "Creates a new vectorof parameter data type of a specific element
   @param lower An integer of the lower bound of the count of a vector
   @param upper An integer of the upper bound of the count of a vector
   @param element-parameters A parameter data type of the elements to be stored in
                             the vector
   @return A vectorof param data type as follows:
           {:type :vectorof
            :range {:lower lower
                    :upper upper}
            :element-type %1
            :element-range %2}"
  ([]
   (println "For a VectorOf, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of element-count: " :integer)
         upper (process-user-input "Upper-bound of element-count: " :integer)]
     (if (and (>= lower 0) (<= lower upper))
       (let [elements (create-parameter-from-user "Now specify the data type of each element of the vector:" "of the vector")]
         (create-new-vectorof-param lower upper elements))
       (do (println "
    INVALID INPUT DETECTED. MAKE SURE LOWER-BOUND IS GREATER THAN OR EQUAL TO
       ZERO (0) AND UPPER-BOUND IS GREATER THAN OR EQUAL TO LOWER-BOUND
                     ")
           (create-new-vectorof-param)))))

  ([lower upper element-parameter]
   (let [element-type (get element-parameter :type)
         element-range (get element-parameter :range)]
     {:type :vectorof
      :range {:lower lower
              :upper upper}
      :element-type element-type
      :element-range element-range})))

(defn create-new-parameter
  "[ABSTRACTION]
   Creates a parameter generator function via hard-coded inputs (meant for use
   in the creation of an Oracle program for human simulation).
   @param type A keyword which signifies which data type to create a parameter data
               type for.
   @param & boundries All of the specifications for the given data type parameter
                     for its creation.
   @return A parameter data type of the specified type with the specified boundries."
  [type & boundries]
  (let [lower (first boundries)
        upper (second boundries)]
    (case type
    :integer (create-new-integer-param lower upper)
    :float (create-new-float-param lower upper)
    :string (let [char-groups (nth boundries 2)
                  unique-chars (nth boundries 3)]
              (create-new-string-param lower upper char-groups unique-chars))
    (let [element-parameter (nth boundries 2)]
      (create-new-vectorof-param lower upper element-parameter)))))

(defn create-parameter-from-user
  "[ABSTRACTION]
   Creates a parameter generator function which is made via user input.
     @return A parameter generator function of specified type. Could be of
             the following:
                 :integer
                 :float
                 :string
                 :vectorof"
  ([param-number]
   (let [choice (do (println "") (process-user-input (str "What is the data type for parameter " param-number "?
    (1) Integer
    (2) Float
    (3) String
    (4) VectorOf
Please choose a number from the options above.") :integer))]
     (case choice
       1 (create-new-integer-param)
       2 (create-new-float-param)
       3 (create-new-string-param)
       4 (create-new-vectorof-param))))

  ([prompt param-for?]
   (println prompt)
   (create-parameter-from-user param-for?)))

(defn acquire-parameters-from-user
  "Will inquire the user to provide parameters for a given problem.
     @return A sequence of parameter generator functions"
  []
  (let [num-params (process-user-input "How many input parameters are given?: " :integer)]
    (loop [input []
           param-count 1]
      (if (<= param-count num-params)
        (recur (conj input (create-parameter-from-user param-count)) (inc param-count))
        input))))

;;-------------------------------------------------------------------------------------;;

;; NOTE: The following functions are responsible for gathering Output Parameters.

(defn acquire-output-type-from-user
  "[HELPER FUNCTION]
   Inquires user for the data type of the output.
     @return A Clojush friendly keyword that corresponds to a specified data type"
  [output-num]
  (let [choice (process-user-input (str "What is the data type for output " output-num " of the program?
    (1) Integer
    (2) Float
    (3) String
    (4) Boolean
    (5) VectorOf
Please choose a number from the options above") :integer)]
    (case choice
      1 :integer
      2 :float
      3 :string
      4 :boolean
      5 (let [vector-elements (process-user-input "What is the data type of each element?
    (1) Integer
    (2) Float
    (3) String                                                       
    (4) Boolean
Please choose a number from the options above" :integer)]
                       (case vector-elements
                         1 :vector_integer
                         2 :vector_float
                         3 :vector_string
                         4 :vector_boolean)))))

(defn acquire-outputs-from-user
  "Inquires user to how many outputs there can be and then prompts user to
   specify each possible output
   @return A vector with each element as a Clojush friendly keyword that corresponds
           to a specified data type"
  []
  (let [num-outputs (process-user-input "
How many outputs there are: " :integer)]
    (loop [outputs []
           output-count 1]
      (if (<= output-count num-outputs)
        (recur (conj outputs (acquire-output-type-from-user output-count))
               (inc output-count))
        outputs))))

;;-------------------------------------------------------------------------------------;;

;; NOTE: The following functions are responsible for generating a random case from the
;;       acquired Input Parameters (it is not responsible for generating associated Output
;;       Parameters, however.)

(defn generate-integer
  "[HELPER FUNCTION]
   Generates a new raw integer parameter based off of boundries
   @param parameter A map which is the parameter data type
   @return An integer that satisifies the range of the given parameter"
  [parameter]
  (let [lower (get-in parameter [:range :lower])
        upper (get-in parameter [:range :upper])
        rand-int (+ lower (rand-int (- upper lower)))]
    rand-int))

(defn generate-float
  "[HELPER FUNCTION]
   Generates a new raw float parameter based off of boundries
   @param parameter A map which is the parameter data type
   @return A float that satisifies the range of the given parameter"
  [parameter]
  (let [lower (get-in parameter [:range :lower])
        upper (get-in parameter [:range :upper])
        rand-float (+ lower (rand (- upper lower)))]
    rand-float))

(defn generate-string
  "[HELPER FUNCTION]
   Generates a new raw string parameter based off of boundries
   @param parameter A map which is the parameter data type
   @return A string that satisifies the range of the given parameter"
  [parameter]
  (let [lower (get-in parameter [:range :lower])
        upper (get-in parameter [:range :upper])
        available-chars (get-in parameter [:range :available-characters])
        length (+ lower (rand-int (inc (- upper lower))))]
    (loop [param ""
           current-char 0]
      (if (< current-char length)
        (recur (str param (#(get % (rand-int (count %))) available-chars))
               (inc current-char))
        param))))

(defn generate-vectorof
  "[HELPER FUNCTION]
     Generates a new raw vectorof parameter based off of boundries
     @param parameter A map which is the parameter data type
     @return A vectorof that satisifies the range and 
             element-range of the given parameter"
  [parameter]
  (let [lower (get-in parameter [:range :lower])
        upper (get-in parameter [:range :upper])
        length (+ lower (rand-int (inc (- upper lower))))
        element {:type (get parameter :element-type)
                 :range (get parameter :element-range)}]
    (loop [vector []]
      (if (< (count vector) length)
        (recur (conj vector (generate-parameter element)))
        vector))))

(defn generate-parameter
  "[ABSTRACTION]
   Creates a random parameter of the given parameter data type
     @param parameter A parameter data type
     @return A random generation of the data type given"
  [parameter]
  (let [type (get parameter :type)]
    (case type
      :integer (generate-integer parameter)
      :float (generate-float parameter)
      :string (generate-string parameter)
      :vectorof (generate-vectorof parameter))))

(defn generate-random-case-input
  "Generates a new case input given a sequence of parameter generator functions.
     @param case-generator A sequence of parameter generator functions
     @return A sequence of case inputs"
  [case-generator]
  (mapv #(generate-parameter %) case-generator))

(defn generate-random-cases
  "Generates a new case input set given a sequence of parameter generator functions.
     @param case-generator A sequence of parameter generator functions
     @param num-cases An integer of the number of cases to return
     @return A case set data type"
  [case-generator num-cases]
  (loop [case-count 0
         cases []]
    (if (< case-count num-cases)
      (recur (inc case-count) (conj cases (vector (generate-random-case-input case-generator) '[])))
      cases)))

;;-------------------------------------------------------------------------------------;;

;; NOTE: The following functions are responsible for gathering Initial Training Cases
;;       from a human

;; NOTE: The following 5 functions are made so the user can shoot themselves in the
;;       foot. However, they are also made to easily change this fact if desired in
;;       in the future.

(defn acquire-specific-input-integer
  "[HELPER FUNCTION]
   Prompts user to give an integer for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  ([parameter parameter-number]
   (let [user-choice (process-user-input (str "Integer " parameter-number ": ") :integer)]
     parameter
     user-choice))

  ([parameter]
   (let [user-choice (process-user-input "Integer: " :integer)]
     parameter
     user-choice)))

(defn acquire-specific-input-float
  "[HELPER FUNCTION]
   Prompts user to give a float for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  ([parameter parameter-number]
   (let [user-choice (process-user-input (str "Float " parameter-number ": ") :float)]
     parameter
     user-choice))

  ([parameter]
   (let [user-choice (process-user-input "Float: " :float)] 
     parameter
     user-choice)))

(defn acquire-specific-input-string
  "[HELPER FUNCTION]
   Prompts user to give a string for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  ([parameter parameter-number]
   (let [user-choice (process-user-input (str "String " parameter-number ": ") :string)]
     parameter
     user-choice))

  ([parameter]
   (let [user-choice (process-user-input "String: " :string)] 
     parameter
     user-choice)))

(defn acquire-specific-input-boolean
  "[HELPER FUNCTION]
   Prompts user to give a boolean for the given case
   @param parameter A parameter data type
   @param parameter-number An integer of the index of the current parameter
   @return A raw parameter of parameter type"
  ([parameter parameter-number]
   (let [user-choice (process-user-input (str "Boolean " parameter-number ": 
    (1) True
    (2) False") :integer)]
     (case user-choice
       1 true
       2 false
       (do
         (println "
    INVALID INPUT DETECTED. ONLY VALID CHOICES ARE (1) AND (2).
                     ")
         (acquire-specific-input-boolean parameter parameter-number)))))

  ([parameter]
   (let [user-choice (process-user-input "Boolean: 
    (1) True
    (2) False" :integer)]
     (case user-choice
       1 true
       2 false
       (do
         (println "
    INVALID INPUT DETECTED. ONLY VALID CHOICES ARE (1) AND (2).
                     ")
         (acquire-specific-input-boolean parameter))))))

(defn acquire-specific-input-vectorof
  "[HELPER FUNCTION]
   Prompts user to give a vectorof for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  [parameter]
  (let [user-vector-length (process-user-input "Vector:
      Element Count: " :integer)
        type (if (keyword? parameter)
               (second (get-output-types parameter))
               (get parameter :element-type))
        element-type (case type
                       :integer (partial acquire-specific-input-integer parameter)
                       :float (partial acquire-specific-input-float parameter)
                       :string (partial acquire-specific-input-string parameter)
                       :boolean (partial acquire-specific-input-boolean parameter))]
    (loop [vector []
           element-count 1]
      (if (<= element-count user-vector-length)
        (recur (conj vector (element-type element-count))
               (inc element-count))
        vector))))

(defn acquire-specific-parameter
  "[HELPER FUNCTION] [ABSTRACTION]
   Prompts user to give a parameter for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  [param-types parameter]
  (let [type (if (= param-types "Inputs")
               (get parameter :type)
               parameter)]
    (case type
      :integer (acquire-specific-input-integer parameter)
      :float (acquire-specific-input-float parameter)
      :string (acquire-specific-input-string parameter)
      :boolean (acquire-specific-input-boolean parameter)
      (acquire-specific-input-vectorof parameter))))

(defn acquire-multiple-params
  "[HELPER FUNCTION] [ABSTRACTION]
   Prompts the user to give a specific parameter for a specified parameter type
   @param param-types A string that is either Inputs or Outputs
   @param types A sequence of parameter data types or keywords which indicate the types
   @return A sequence of raw parameters"
  [param-types types]
  (println "---------------------------------------------------------
" param-types "
               ")
  (let [num-params (count types)]
    (loop [params []
           param-count 1]
      (if (<= param-count num-params)
        (recur (conj params (acquire-specific-parameter param-types (nth types (dec param-count))))
               (inc param-count))
        (do (println "---------------------------------------------------------")
            params)))))

(defn acquire-training-inputs
  "[HELPER FUNCTION]
   Prompts user to give training inputs for a given case
   @param types A sequence of parameter data type
   @return A sequence of raw parameters of parameter type"
  [types]
  (acquire-multiple-params "Inputs" types))

(defn acquire-training-outputs
  "[HELPER FUNCTION]
   Prompts user to give training outputs for a given case
   @param types A sequence of parameter data type
   @return A sequence of raw parameters of parameter type"
  [types]
  (acquire-multiple-params "Outputs" types))

(defn acquire-training-case-from-user
  "[HELPER FUNCTION]
   Prompts user to give training cases
   @param input-types A sequence of parameter data types for the inputs of the case
   @param output-types A sequence of parameter data types for the outputs of the case
   @return A sequence of two sequences where the first element is the inputs and
           the second is the outputs"
  [input-types output-types]
  (conj [] (acquire-training-inputs input-types) (acquire-training-outputs output-types)))

(defn get-initial-training-cases-from-user
  "Prompts the user to give n number of initial training cases
   @param input-types A sequence of parameter data types
   @param output-types A sequence of Clojush friendly keywords
   @param num-cases An integer of the number of initial cases requested
   @return A sequence (initial training cases) where each element is a sequence (initial training case)
           that contains two sequences each (Inputs & Outputs)
           i.e. [[[inputs-1] [outputs-1]]
                 .....................
                 [[inputs-n] [outputs-n]]]"
  [input-types output-types num-cases]
  (println "
Please provide some example training cases for the GP run to use.
            ")
  (loop [initial-cases []
         case-count 1]
    (if (<= case-count num-cases)
      (do (println "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Case #" case-count ":
                              ")
          (recur (conj initial-cases (acquire-training-case-from-user input-types output-types))
                 (inc case-count)))
      (do (println "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
          initial-cases))))

;;-------------------------------------------------------------------------------------;;

;; NOTE: The following functions are responisble for defining an atom generator
;;       based on information provided from functions above and human guidance.

(defn acquire-atom-generator-push-stacks
  "Prompts user to provide the required register of stacks for the given GP run
   @param1 invalid-stack A keyword that represents a stack which does not occur within clojush.global/push-types
   @return A vector of keywords which are within clojush.global/push-types
           that should be registered for use within a given GP run."
  ([]
   (println "
Which stacks should be registered to solve this problem?

***Please enter your stacks and seperate via a space
character \" \" if more than one stack is required (Execution
stack is already included)***")
   (let [choices (clojure.string/split (read-line) #" ")]
     (conj (loop [register-stacks []
                  choice-index 0]
             (if (< choice-index (count choices))
               (let [choice (nth choices choice-index)
                     stack (keyword choice)]
                 (if (some #(= stack %) clojush.globals/push-types)
                   (recur (conj register-stacks stack) (inc choice-index))
                   (recur (let [correct-stack (acquire-atom-generator-push-stacks stack)]
                            (if (= correct-stack "")
                              register-stacks
                              (conj register-stacks correct-stack))) (inc choice-index))))
               register-stacks)) :exec)))

  ([invalid-stack]
   (println "
ERROR: " invalid-stack " is not a recognized stack. Please correct this input
(if you wish to omit this stack entirely, please press enter only).
")
   (let [raw-input (read-line)
         correction (if (= raw-input "")
                      raw-input
                      (keyword raw-input))]
     (if (= correction "")
       correction
       (if (some #(= correction %) clojush.globals/push-types)
         correction
         (acquire-atom-generator-push-stacks correction))))))

(defn acquire-input-instructions
  "Gathers required symbols for input instructions for the human
   driven atom generator
   @param input A vector of parameter data types acquired from acquire-parameters-from-user
   @return A list of symbols in numerical order
           i.e. ('in1 'in2 ... 'inn)"
  [input]
  (loop [input-instructions ()
         input-index 0]
    (if (< input-index (count input))
      (recur (conj input-instructions (symbol (str "in" (inc input-index)))) (inc input-index))
      input-instructions)))

(defn acquire-atom-generator-constants
  "Prompts the user to enter any constants the atom generator may use.
   @param registered-stacks A vector of Push stacks being used
   @return A vector of constants to use for atom generators"
  [registered-stacks]
  (let [constants {:integer [-1 0 1]
                   :float [-1.0 0.0 1.0]
                   :boolean [false true]
                   :char [\space \newline]
                   :string [""]
                   :vector_integer [[] [0]]
                   :vector_float [[] [0.0]]
                   :vector_boolean [[]]
                   :vector_string [[] [""]]}
        suggested-constants (vec (filter #(not (nil? %)) (map #(second (find constants %)) registered-stacks)))]
    (println "Here are a list of suggested constants based on the stacks registered for this GP run: ")
    (doseq [consts suggested-constants]
      (prn consts))
    (let [user-input (process-user-input "Add all of these as constants? (Y/N)" :string)
          constants-to-add (cond
                             (or (= user-input "Y") (= user-input "y")) (apply concat suggested-constants)
                             (or (= user-input "N") (= user-input "n")) [])]
      (println "Please enter any other specific constants for this GP run
(if entering a string or character, surround the string in
quotes or add a \\ before the character respectively).")
      (concat constants-to-add (apply concat (for [current-stack registered-stacks]
                                               (when (find constants current-stack)
                                                 (loop [stack-constants []
                                                        constant-count 1]
                                                   (if (not (= (last stack-constants) ""))
                                                     (do
                                                       (println current-stack " " constant-count ": ")
                                                       (recur (conj stack-constants (read-line)) (inc constant-count)))
                                                     (case current-stack
                                                       :integer (map #(Integer/parseInt %) (take (dec (count stack-constants)) stack-constants))
                                                       :float (map #(Float/parseFloat %) (take (dec (count stack-constants)) stack-constants))
                                                       :char (map #(read-string %) (take (dec (count stack-constants)) stack-constants))
                                                       :string (map #(read-string %) (take (dec (count stack-constants)) stack-constants))
                                                       :boolean (map #(if (or (= % "True") (= % "true") (= % "TRUE") (= % "t") (= % "T"))
                                                                        true
                                                                        false) (take (dec (count stack-constants)) stack-constants))
                                                       nil))))))))))

(comment 
  (= (acquire-parameters-from-user) [(create-new-parameter :integer 1 1000000) (create-new-parameter :integer 1 1000000)])
  )