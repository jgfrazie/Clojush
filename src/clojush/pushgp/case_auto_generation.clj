(ns clojush.pushgp.counterexample-driven-gp
  (:use [clojush random args pushstate interpreter globals individual util]))

(declare create-parameter-from-user)

(defn add-groups-to-str
  "Will add a predefined group of chars to a string
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
  "Displays prompt for the user and will convert input to a specified data type
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
                                                 :upper upper}, :param #}"
  ([]
   (println "For an Integer, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of intger range:" :integer)
         upper (process-user-input "Upper-bound of integer range:" :integer)]
     #(create-new-integer-param lower upper)))

  ([lower upper]
   (let [rand-param (+ lower (rand-int (inc (- upper lower))))]
     {:type :integer
      :range {:lower lower
              :upper upper}
      :param rand-param})))

(defn create-new-float-param
  "Creates a new float parameter type with a range specified by the user
   @param1 lower A float of the lower bound of the range of the parameter
   @param1 upper A float of the upper bound of the range of the parameter
   @return A map of the following format: {:type :float, :range {:lower, lower,
           :upper upper}, :param #}"
  ([]
   (println "For a Float, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of float range:" :float)
         upper (process-user-input "Upper-bound of float range:" :float)]
     #(create-new-float-param lower upper)))

  ([lower upper]
   (let [rand-param (+ lower (rand (- upper lower)))]
     {:type :float
      :range {:lower lower
              :upper upper}
      :param rand-param})))

(defn get-char-sets
  "A helper function which aquires character grouping selections from the user.
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
  "A helper function which aquires extra possible characters from the user which could
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
           :upper upper}, :available-characters #1}, :param #2}"
  ([]
   (println "For a String, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of string length: " :integer)
         upper (process-user-input "Upper-bound of string length: " :integer)
         char-sets (get-char-sets)
         extras (get-extra-chars)]
     #(create-new-string-param lower upper char-sets extras)))

  ([lower upper char-groups unique-chars]
   (let [length (+ lower (rand-int (inc (- upper lower))))]
     (if (string? char-groups)
       (let [available-chars char-groups]
         {:type :string
          :range {:lower lower :upper upper
                  :available-characters available-chars}
          :param (create-new-string-param length available-chars)}))

     (let [available-chars (add-groups-to-str (reduce str unique-chars) char-groups)]
       {:type :string
        :range {:lower lower :upper upper
                :available-characters available-chars}
        :param (create-new-string-param length available-chars)})))
  
  ([length available-chars]
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
   @param generator-function An anon function which already has the function and params
                             for the element of the vector
   @return A vectorof param data type as follows:
           {:type :vectorof
            :range {:lower lower
                    :upper upper}
            :element-type %1
            :element-range %2
            :param %3}"
  ([]
   (println "For a VectorOf, please provide the following information.")
   (let [lower (process-user-input "Lower-bound of element-count: " :integer)
         upper (process-user-input "Upper-bound of element-count: " :integer)
         generator (create-parameter-from-user "Now specify the data type of each element of the vector:")]
     #(create-new-vectorof-param lower upper generator)))

  ([lower upper generator-function]
   (let [length (+ lower (rand-int (inc (- upper lower))))
         element-type (get (generator-function) :type)
         element-range (get (generator-function) :range)]
     {:type :vectorof
      :range {:lower lower
              :upper upper}
      :element-type element-type
      :element-range element-range
      :param (loop [param []
                    index 0]
               (if (< index length)
                 (recur (conj param (get (generator-function) :param)) (inc index))
                 param))})))
  
  (defn create-parameter-from-user
    "Creates a parameter generator function which is made via user input.
     @return A parameter generator function of specified type. Could be of
             the following:
                 :integer
                 :float
                 :string
                 :vectorof"
    ([]
    (let [choice (process-user-input "What is the data type for this parameter?
    (1) Integer
    (2) Float
    (3) String
    (4) VectorOf
Please choose a number from the options above" :string)]
      (cond
        (= choice "1") (create-new-integer-param)
        (= choice "2") (create-new-float-param)
        (= choice "3") (create-new-string-param)
        (= choice "4") (create-new-vectorof-param))))
    
    ([prompt]
     (println prompt)
     (create-parameter-from-user)))
  
  (defn aquire-parameters-from-user
    "Will inquire the user to provide parameters for a given problem.
     @return A sequence of parameter generator functions"
    []
    (let [num-params (process-user-input "How many parameters are given?: " :integer)]
      (loop [input []
             param-count 0]
        (if (< param-count num-params)
          (recur (conj input (create-parameter-from-user)) (inc param-count))
          input))))
  
  (defn aquire-output-type-from-user
    "Inquires user for the data type of the output.
     @return The keywords :integer, :float, or :string if it is only that, or
             returns a vector with the following format: [:vectorof %]"
    []
    (let [choice (process-user-input "What is the data type for the output of the program?
    (1) Integer
    (2) Float
    (3) String
    (4) VectorOf
Please choose a number from the options above" :string)]
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
                           (= vector-elements "1") [:vectorof :integer]
                           (= vector-elements "2") [:vectorof :float]
                           (= vector-elements "3") [:vectorof :string])))))
  
  (defn generate-case
    "Generates a new case input given a sequence of parameter generator functions.
     @param case-generator A sequence of parameter generator functions
     @return A sequence of case inputs"
    [case-generator]
    (mapv #(%) case-generator))

  (comment
    (create-new-vectorof-param 5 7 #(create-new-integer-param -127 100))
    (create-new-vectorof-param 1 3 #(create-new-float-param 5 6))
    (create-new-vectorof-param 4 8 #(create-new-string-param 3 5 [:digits] ["?" "." "!"]))
    (create-parameter-from-user)
    ((create-parameter-from-user))
    (aquire-output-type-from-user)
    (generate-case (aquire-parameters-from-user))
    )