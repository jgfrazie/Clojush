(ns clojush.pushgp.counterexample-driven-gp
  (:use [clojush random args pushstate interpreter globals individual util]))

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
             (= %2 :digits)     (str %1 "0123456789"))
          string
          groups))

(defn create-new-integer-param
  "Creates a new integer parameter type with a range specified by the user
   @param lower An integer of the lower bound of the range of the parameter
   @param upper An integer of the upper bound of the range of the parameter
   @return A map of the following format: {:type :integer, :range {:lower lower,
                                                 :upper upper}, :param #}"
  [lower upper]
  (def rand-param (+ lower (rand-int (inc (- upper lower)))))
  {:type :integer
   :range {:lower lower
           :upper upper}
   :param rand-param})

(defn create-new-float-param
  "Creates a new float parameter type with a range specified by the user
   @param lower A float of the lower bound of the range of the parameter
   @param upper A float of the upper bound of the range of the parameter
   @return A map of the following format: {:type :float, :range {:lower, lower,
           :upper upper}, :param #}"
  [lower upper]
  (def rand-param (+ lower (rand (- upper lower))))
  {:type :float
   :range {:lower lower
           :upper upper}
   :param rand-param})

(defn create-new-string-param
  "Creates a new string parameter type with a length range specified by the user
   @param lower An integer of the lower-bound of the string length
   @param upper An integer of the upper-bound of the string length
   @param1 char-groups A vector of the following possible key-words to add to the possible string combination:
          :lower-case
          :upper-case
          :digits
          :specials
   @param2 char-groups A string of the possible characters for each character of the string param
   @param & unique-chars A listing of all the other characters to be added to the
                         possible char creation of the string that were not otherwise specified
   @return A map of the following format: {:type :string, :range {:length {:lower lower,
           :upper upper}, :available-characters #1}, :param #2}"
  ([lower upper char-groups & unique-chars]
   (def length (+ lower (rand-int (inc (- upper lower)))))
   (if (string? char-groups)
     (def available-chars char-groups)
     (def available-chars (add-groups-to-str (reduce str unique-chars) char-groups)))

  {:type :string
   :range {:bounds {:lower lower :upper upper}
           :available-characters available-chars}
   :param (create-new-string-param length available-chars)})
  
  ([length available-chars]
   (loop [param ""
          current-char 0]
     (if (< current-char length)
       (recur (str param (#(get % (rand-int (count %))) available-chars))
              (inc current-char))
       param))))

(defn create-new-vectorof-param
  "Creates a new vectorof parameter data type of a specific element"
  [lower upper generator-function]
  (def length (+ lower (rand-int (inc (- upper lower)))))
  (loop [param []
         index 0]
    (if (< index length)
      (recur (conj param (generator-function)) (inc index))
      param)))

(comment
  (create-new-string-param 2 5 [] "+" "-" "_")
  (get (create-new-string-param 3 4 [:upper-case]) :param)
  (create-new-string-param 2 5 "ABCDE")

  (get (create-new-integer-param 0 1) :param)
  (create-new-integer-param -127 100)

  (get (create-new-float-param 0 1) :param)
  (create-new-float-param 0.5 0.6)

  (add-groups-to-str "HEYO " [:lower-case :upper-case :digits :specials])

  (partial (create-new-integer-param -127 100))

  (create-new-vectorof-param 5 7 #(create-new-integer-param -127 100))
  (create-new-vectorof-param 1 3 #(create-new-float-param 5 6))
  (create-new-vectorof-param 4 8 #(create-new-string-param 3 16 [:lower-case :digits] "?" "." "!"))
  )