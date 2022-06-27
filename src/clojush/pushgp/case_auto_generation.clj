"File: case_auto_generation.clj
 Desc: Creates parameter data types either via coder or human input.
       Will also generate a completly new case input from given parameters
       when prompted.
       Will acquire output from human user as well.
 Main-Functions: acquire-outputs-from-user
                 acquire-parameters-from-user
                 create-parameter
                 generate-case-input
                 generate-parameter
                 get-initial-training-cases-from-user
 Author: James Frazier
 Date-Last-Edited: June 21, 2022"

(ns clojush.pushgp.case-auto-generation
  (:require [clojure.math.numeric-tower :as math]))

(declare generate-parameter)
(declare create-parameter-from-user)

(defn vector-of-number-difference
  "Implements distances between vectors of numbers. Alg:
   Add the difference in length between the program's output vector and the
   correct vector times 1000 to the absolute difference between each integer
   and the corresponding integer in the correct vector."
  [vecA vecB]
  (+ (* 1000 (math/abs (- (count vecA) (count vecB))))
     (apply +
            (map #(math/abs (- %1 %2))
                 vecA
                 vecB))))

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
  (reduce #(cond
             (= %2 :lower-case) (str %1 "abcdefghijklmnopqrstuvwxyz")
             (= %2 :upper-case) (str %1 "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
             (= %2 :specials)   (str %1 " !@#$%^&*()_+-=`~,<.>/?]}[{")
             (= %2 :digits)     (str %1 "0123456789")
             :else              %1)
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
  (cond
    (= type :integer) (Integer/parseInt (read-line))
    (= type :string)  (read-line)
    (= type :float)   (Float/parseFloat (read-line))))

(defn create-new-integer-param
  "Creates a new integer parameter type with a range specified by the user
   @param1 lower An integer of the lower bound of the range of the parameter
   @param1 upper An integer of the upper bound of the range of the parameter
   @return A map of the following format: {:type :integer, :range {:lower lower,
                                                                   :upper upper}}"
  ([]
   (println "For an Integer, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of intger range:" :integer)
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
      (cond
        (= choice "0") nil
        (= choice "1") :lower-case
        (= choice "2") :upper-case
        (= choice "3") :digits
        (= choice "4") :specials))))

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
                :available-characters available-chars}})) 
     (let [available-chars (add-groups-to-str (reduce str unique-chars) char-groups)]
       {:type :string
        :range {:lower lower :upper upper
                :available-characters available-chars}})))

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
    (cond
      (= type :integer) (generate-integer parameter)
      (= type :float) (generate-float parameter)
      (= type :string) (generate-string parameter)
      (= type :vectorof) (generate-vectorof parameter))))
  
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
Please choose a number from the options above.") :string))]
     (cond
       (= choice "1") (create-new-integer-param)
       (= choice "2") (create-new-float-param)
       (= choice "3") (create-new-string-param)
       (= choice "4") (create-new-vectorof-param))))

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
  
(defn acquire-output-type-from-user
  "[HELPER FUNCTION]
   Inquires user for the data type of the output.
     @return A Clojush friendly keyword that corresponds to a specified data type"
  [output-num]
  (let [choice (process-user-input (str "What is the data type for output " output-num " of the program?
    (1) Integer
    (2) Float
    (3) String
    (4) VectorOf
Please choose a number from the options above") :string)]
    (cond
      (= choice "1") :integer
      (= choice "2") :float
      (= choice "3") :string
      (= choice "4") (let [vector-elements (process-user-input "What is the data type of each element?
    (1) Integer
    (2) Float
    (3) String
Please choose a number from the options above" :string)]
                       (cond
                         (= vector-elements "1") :vector_integer
                         (= vector-elements "2") :vector_float
                         (= vector-elements "3") :vector_string)))))

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
  
(defn generate-case-input
  "Generates a new case input given a sequence of parameter generator functions.
     @param case-generator A sequence of parameter generator functions
     @return A sequence of case inputs"
  [case-generator]
  (mapv #(generate-parameter %) case-generator))

;;-------------------------------------------------------------------------------------;;

;;NOTE: The following 4 functions are made so the user can shoot themselves in the
;;      foot. However, they are also made to easily change this fact for any choice in
;;      in the future.

(defn acquire-specific-input-integer
  "[HELPER FUNCTION]
   Prompts user to give an integer for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type" 
  ([parameter parameter-number]
   (let [user-choice (process-user-input (str "Integer " parameter-number ": ") :integer)]
     user-choice))

  ([parameter] 
   (let [user-choice (process-user-input "Integer: " :integer)] 
     user-choice)))

(defn acquire-specific-input-float
  "[HELPER FUNCTION]
   Prompts user to give a float for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  ([parameter parameter-number]
   (let [user-choice (process-user-input (str "Float " parameter-number ": ") :float)]
     user-choice))

  ([parameter] 
   (let [user-choice (process-user-input "Float: " :float)] 
     user-choice)))

(defn acquire-specific-input-string
  "[HELPER FUNCTION]
   Prompts user to give a string for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  ([parameter parameter-number]
   (let [user-choice (process-user-input (str "String " parameter-number ": ") :string)]
     user-choice))

  ([parameter] 
   (let [user-choice (process-user-input "String: " :string)] 
     user-choice)))

(defn acquire-specific-input-vectorof
  "[HELPER FUNCTION]
   Prompts user to give a vectorof for the given case
   @param parameter A parameter data type
   @return A raw parameter of parameter type"
  [parameter]
  (let [user-vector-length (process-user-input "Vector:
      Element Count: " :integer)
        type (get parameter :element-type)
        element-type (cond
                  (= type :integer) (partial acquire-specific-input-integer parameter)
                  (= type :float) (partial acquire-specific-input-float parameter)
                  (= type :string) (partial acquire-specific-input-string parameter))]
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
    (cond
      (= type :integer) (acquire-specific-input-integer parameter)
      (= type :float) (acquire-specific-input-float parameter)
      (= type :string) (acquire-specific-input-string parameter)
      :else (acquire-specific-input-vectorof parameter))))

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

;;NOTE: Should acquire both the input and output for a specific case from the user
;;      and place them in a vector
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
  (println "")
  (println "Please provide some example training cases for the GP run to use.
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

(comment 
  (get-initial-training-cases-from-user (acquire-parameters-from-user) (acquire-outputs-from-user) 2)
  )
