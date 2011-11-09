;; synthesis.core.clj
;; The main code for query creation.
;; Tom Hemuth, thelmuth@cs.umass.edu, 2011

(ns synthesis.core
  (:require [clojure.contrib.sql :as sql]
            [clojush]
            [synthesis.db :as db]))

;;;;;;;;;;;;
;; A few things must be done in the clojush namespace.
(in-ns 'clojush)

;; Redefine push-types to include :select, :from, and :where, and then redefine the push state structure.
(def push-types '(:exec :integer :float :code :boolean :auxiliary :tag :zip :string :select :from :where))
(define-push-state-structure)

;;;;;;;;;;;;
;; Return to core namespace
(in-ns 'synthesis.core)

;;;;;;;;;;
;; Globals for creating constraints

(def comparators ["=" "<" ">" "<=" ">=" "<>"])
(def cols-map db/synthesis-db-columns-map)

;;;;;;;;;;
;; Normal stack instructions for SFW
(define-registered where_dup (duper :where))
(define-registered where_swap (swapper :where))
(define-registered where_rot (rotter :where))
; Other possibilities: pop, flush, eq, stackdepth, yank, yankdup, shove

;;;;;;;;;;
;; Helper functions for :where stack manipulation

(defn select-column
  "Returns column name based on index. Index is constrained within bounds of vector length by modulus."
  [index]
  (first (nth db/synthesis-db-columns (mod index (count db/synthesis-db-columns)))))

(defn get-column-type
  "Retrieves the column type for column."
  [column]
  (let [type (get cols-map column)]
    (cond
      (= type :int) :integer
      (string? type) (if (= "varchar" (subs type 0 7))
                       :string
                       nil)
      true nil)))

(defn get-constant-from-column
  "Gets a constant from column from row index. Optional argument distinct is a boolean that defines whether
   or not the values in the column should be distinct. Index is taken mod the number of options to always
   give a legal index."
  [column index distinct]
    (let [query (str "SELECT "
                     (if distinct
                       "DISTINCT "
                       "")
                     (name column)
                     " FROM adult")
          results (db/run-db-function db/synthesis-db db/db-query query)]
      (get (nth results
                (mod index (count results)))
           column)))

;;;;;;;;;;
;; Instructions for :where stack manipulation

(clojush/define-registered where_constraint_from_stack
                           (fn [state]
                             (if (not (>= (count (get state :integer)) 2)) ; We will need at least 2 integers
                               state
                               (let [column (select-column (clojush/stack-ref :integer 0 state))
                                     column-type (get-column-type column)]
                                 (if (nil? column-type) ; Check for legit column-type
                                   state
                                   (if (or (empty? (get state column-type)) ; Make sure column type isn't empty
                                           (and (= column-type :integer)
                                                (not (>= (count (get state :integer)) 3))))
                                     state
                                     (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                                            (count comparators)))
                                           constant (if (= column-type :integer)
                                                      (clojush/stack-ref :integer 2 state)
                                                      (clojush/stack-ref column-type 0 state))
                                           constraint (str (name column) " " comparator " " constant)]
                                       (clojush/push-item constraint :where
                                                          (clojush/pop-item :integer
                                                                            (clojush/pop-item :integer
                                                                                              (clojush/pop-item column-type
                                                                                                                state)))))))))))

; Uses a constant taken from the selected column, where options are not distinct.
; This makes probability of choosing constant C proportional to the frequency of C in the column.
(clojush/define-registered where_constraint_from_index
                           (fn [state]
                             (if (not (>= (count (get state :integer)) 3)) ; We will need 3 integers
                               state
                               (let [column (select-column (clojush/stack-ref :integer 0 state))
                                     column-type (get-column-type column)]
                                 (if (nil? column-type) ; Check for legit column-type
                                   state
                                   (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                                          (count comparators)))
                                         constant (get-constant-from-column column
                                                                            (clojush/stack-ref :integer 2 state)
                                                                            false)
                                         constraint (str (name column) " " comparator " " constant)]
                                     (clojush/push-item constraint :where
                                                        (clojush/pop-item :integer
                                                                          (clojush/pop-item :integer
                                                                                            (clojush/pop-item :integer
                                                                                                              state))))))))))

; Uses a constant taken from the selected column, where options are distinct.
; This makes each distinct option equally likely.
(clojush/define-registered where_constraint_distinct_from_index
                           (fn [state]
                             (if (not (>= (count (get state :integer)) 3)) ; We will need 3 integers
                               state
                               (let [column (select-column (clojush/stack-ref :integer 0 state))
                                     column-type (get-column-type column)]
                                 (if (nil? column-type) ; Check for legit column-type
                                   state
                                   (let [comparator (nth comparators (mod (clojush/stack-ref :integer 1 state)
                                                                          (count comparators)))
                                         constant (get-constant-from-column column
                                                                            (clojush/stack-ref :integer 2 state)
                                                                            true)
                                         constraint (str (name column) " " comparator " " constant)]
                                     (clojush/push-item constraint :where
                                                        (clojush/pop-item :integer
                                                                          (clojush/pop-item :integer
                                                                                            (clojush/pop-item :integer
                                                                                                              state))))))))))

;;;;;;;;;;
;; Query from Examples
;;
;; SFW maps are of the following form:
;; :select ["column1" "column2" "column3" ... "columnN"]
;; :from ["table1" "table2" ... "tableM"]
;; :where ["clause1" 'AND/'OR "clause2" 'AND/'OR ... "clauseZ"]

(defn sfw-map-to-query-string
  "Takes a map of a SFW query and returns a string representation of that query."
  [swf-map]
  (str "SELECT " (apply str (interpose ", " (get swf-map :select))) \newline
       "FROM " (apply str (interpose ", " (get swf-map :from))) \newline
       "WHERE " (apply str (interpose " " (get swf-map :where)))))

#_(clojush/pushgp :error-function (fn [program]
                                  (list
                                    (let [embryo-query {:select []
                                                        :from []
                                                        :where []}
                                          final-state (clojush/run-push program
                                                                        (clojush/push-item embryo-query
                                                                                           :auxiliary
                                                                                           (clojush/make-push-state)))
                                          result-query (clojush/top-item :auxiliary final-state)]
                                      ;Now, need to create a SFW string, and use it on the database to find the fitness.
                                      ;----for now, just return a random integer
                                      (rand-int 1000))))
                :atom-generators (list 'string_length
                                       'string_take
                                       'string_concat
                                       'and_constraint)
                :population-size 100
                :max-generations 50
                :tournament-size 7)



;; Test sfw-map-to-query-string
#_(def ex-query
  (sfw-map-to-query-string {:select ["age" "education" "hours_per_week"]
                            :from ["adult"]
                            :where ["age > 50" 'AND "workclass = \"State-gov\""]}))

#_(db/run-db-function db/synthesis-db db/db-query ex-query)

;; To get an indexed thing from the distinct things
#_(nth (db/run-db-function db/synthesis-db db/db-query "SELECT DISTINCT education
                                                           FROM adult
                                                           WHERE 0=0") 8)

;; To get an indexed thing from non-distinct things
#_(nth (db/run-db-function db/synthesis-db db/db-query "SELECT education
                                                           FROM adult
                                                           WHERE 0=0") 24212)

;; Test and_constraint
(println (clojush/run-push '(24 "thomas" 2 1 where_constraint_from_stack)
                           (clojush/make-push-state)))

(println (clojush/run-push '(4 "thomas" 2 0 where_constraint_from_index)
                           (clojush/make-push-state)))

(println (clojush/run-push '(7 "thomas" 2 0 where_constraint_distinct_from_index)
                           (clojush/make-push-state)))
