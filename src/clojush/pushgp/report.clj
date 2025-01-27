(ns clojush.pushgp.report
  (:use [clojush util globals pushstate simplification individual]
        [clojush.pushgp counterexample-driven-gp]
        [clojure.data.json :only (json-str)])
  (:require [clojure.string :as string]
            [config :as config]
            [clj-random.core :as random]
            [local-file]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojush.pushgp.record :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions

(defn git-last-commit-hash
  "Returns the last Git commit hash"
  []
  (let [dir (local-file/project-dir)]
    (string/trim
      (slurp
        (str dir
             "/.git/"
             (subs
               (string/trim
                 (slurp
                   (str dir "/.git/HEAD")))
               5))))))

(defn print-params [push-argmap]
  (doseq [[param val] push-argmap]
    (println (name param) "="
             (cond
               (= param :random-seed) (random/seed-to-string val)
               (= param :training-cases) (pr-str val)
               (= param :sub-training-cases) (pr-str val)
               :else val))))

(defn print-genome
  [individual argmap]
  (pr-str (not-lazy
           (if (= :plush (:genome-representation argmap))
             (map #(dissoc % :uuid :parent-uuid) (:genome individual))
             (:genome individual)))))

(defn behavioral-diversity
  "Returns the behavioral diversity of the population, as described by David
   Jackson in 'Promoting phenotypic diversity in genetic programming'. It is
   the percent of distinct behavior vectors in the population."
  [population]
  (float (/ (count (distinct (map :behaviors population)))
            (count population))))

(defn sample-population-edit-distance
  "Returns a sample of Levenshtein distances between programs in the population,
   where each is divided by the length of the longer program."
  [pop samples]
  (let [instr-programs (map #(map :instruction %)
                            (map :genome pop))]
    (repeatedly samples
                #(let [prog1 (random/lrand-nth instr-programs)
                       prog2 (random/lrand-nth instr-programs)
                       longer-length (max (count prog1) (count prog2))]
                   (if (zero? longer-length)
                     0
                     (float (/ (levenshtein-distance prog1 prog2)
                               longer-length)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log printing (csv and json)

(defn csv-print
  "Prints a csv of the population, with each individual's fitness and size.
   If log-fitnesses-for-all-cases is true, it also prints the value
   of each fitness case."
  [population generation {:keys [csv-log-filename csv-columns genome-representation]}]
  (let [columns (vec
                 (map
                  (fn [col] (if (= col :genome-closes)
                              (if (= genome-representation :plush)
                                :plush-genome-closes
                                :plushy-genome-closes)
                              col))
                  (concat [:uuid]
                          (filter #(some #{%} csv-columns)
                                  [:generation :location :parent-uuids :genetic-operators
                                   :push-program-size :plush-genome-size :push-program
                                   :plush-genome :total-error :is-random-replacement
                                   :genome-closes :push-paren-locations]))))]
    (when (zero? generation)
      (with-open [csv-file (io/writer csv-log-filename :append false)]
        (csv/write-csv csv-file
                       (vector (concat (map name columns)
                                       (when (some #{:test-case-errors} csv-columns)
                                         (map #(str "TC" %)
                                              (range (count (:errors (first population)))))))))))
    (with-open [csv-file (io/writer csv-log-filename :append true)]
      (csv/write-csv
        csv-file
        (map-indexed
          (fn [location individual]
            (concat (map (assoc (into {} individual)
                           :generation generation
                           :location location
                           :parent-uuids (let [parent-uuids (not-lazy
                                                              (map str (:parent-uuids individual)))]
                                           (if (empty? parent-uuids)
                                             []
                                             parent-uuids))
                           :genetic-operators (if (nil? (:genetic-operators individual))
                                                []
                                                (:genetic-operators individual))
                           :push-program-size (count-points (:program individual))
                           :push-program (if (and (seq? (:program individual))
                                                  (empty? (:program individual)))
                                           "()"
                                           (:program individual))
                           :plush-genome-size (count (:genome individual))
                           :plush-genome (if (empty? (:genome individual))
                                           "()"
                                           (not-lazy (:genome individual)))
                           :plush-genome-closes (if (empty? (:genome individual))
                                                  "()"
                                                  (apply str
                                                         (not-lazy
                                                          (map (fn [closes]
                                                                 (if (<= closes 9)
                                                                   closes
                                                                   (str "<" closes ">")))
                                                               (map :close (:genome individual))))))
                           :plushy-genome-closes (if (empty? (:genome individual))
                                                   "()"
                                                   (apply str
                                                          (not-lazy (map (fn [instr]
                                                                           (if (= instr :close)
                                                                             1
                                                                             0))
                                                                         (:genome individual)))))
                           :push-paren-locations (if (empty? (:genome individual))
                                                   ""
                                                   (apply str
                                                          (not-lazy
                                                           (map #(case %
                                                                   :open "("
                                                                   :close ")"
                                                                   "-")
                                                                (list-to-open-close-sequence (:program individual))))))
                           ) ; This is a map of an individual
                         columns)
                    (when (some #{:test-case-errors} csv-columns)
                      (:errors individual))))
          population)))))

(defn edn-print
  "Takes a population and appends all the individuals to the EDN log file.
   If the internal representation of individuals changes in future versions
   of Clojush, this code will likely continue to work, but will produce
   output corresponding to the new representation."
  [population generation edn-log-filename keys additional-keys]
  (with-open [w (io/writer edn-log-filename :append true)] ;; Opens and closes the file once per call
    (doall
     (map-indexed (fn [index individual]
                   (let [additional-data {:generation generation
                                          :location index
                                          :push-program-size (count-points (:program individual))
                                          :plush-genome-size (count (:genome individual))}]
                     (.write w "#clojush/individual")
                     (.write w (prn-str (merge
                                         (select-keys additional-data additional-keys)
                                         (select-keys individual keys))))))
          population))))

(defn jsonize-individual
  "Takes an individual and returns it with only the items of interest
   for the json logs."
  [log-fitnesses-for-all-cases json-log-program-strings generation individual]
  (let [part1-ind (-> (if log-fitnesses-for-all-cases
                        {:errors (:errors individual)}
                        {})
                      (assoc :total-error (:total-error individual))
                      (assoc :generation generation)
                      (assoc :size (count-points (:program individual))))
        part2-ind (if json-log-program-strings
                    (assoc part1-ind :program (str (not-lazy (:program individual))))
                    part1-ind)
        part3-ind (if (:weighted-error individual)
                    (assoc part2-ind :weighted-error (:weighted-error individual))
                    part2-ind)]
    part3-ind))

(defn json-print
  "Prints a json file of the population, with each individual's fitness and size.
   If log-fitnesses-for-all-cases is true, it also prints the value
   of each fitness case."
  [population generation json-log-filename log-fitnesses-for-all-cases
   json-log-program-strings]
  (let [pop-json-string (json-str (map #(jsonize-individual
                                          log-fitnesses-for-all-cases
                                          json-log-program-strings
                                          generation
                                          %)
                                       population))]
   (if (zero? generation)
     (spit json-log-filename (str pop-json-string "\n") :append false)
     (spit json-log-filename (str "," pop-json-string "\n") :append true))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; report printing functions

(defn lexicase-report
  "This extra report is printed whenever lexicase selection is used."
  [population {:keys [error-function report-simplifications print-errors
                      print-history meta-error-categories
                      print-lexicase-best-programs] :as argmap}]

  (let [min-error-by-case (apply map
                                 (fn [& args] (apply min args))
                                 (map :errors population))
        lex-best (apply max-key
                        (fn [ind]
                          (apply + (map #(if (== %1 %2) 1 0)
                                        (:errors ind)
                                        min-error-by-case)))
                        population)
        pop-elite-by-case (map (fn [ind]
                                 (map #(if (== %1 %2) 1 0)
                                      (:errors ind)
                                      min-error-by-case))
                               population)
        count-elites-by-case (map #(apply + %) (apply mapv vector pop-elite-by-case))
        most-zero-cases-best (apply max-key
                                    (fn [ind]
                                      (apply + (map #(if (zero? %) 1 0)
                                                    (:errors ind))))
                                    population)
        pop-zero-by-case (map (fn [ind]
                                (map #(if (zero? %) 1 0)
                                     (:errors ind)))
                              population)
        count-zero-by-case (map #(apply + %) (apply mapv vector pop-zero-by-case))]

    (when print-lexicase-best-programs
      (println "--- Lexicase Program with Most Elite Cases Statistics ---")
      (println "Lexicase best genome:" (print-genome lex-best argmap))
      (println "Lexicase best program:" (pr-str (not-lazy (:program lex-best))))
      (when (> report-simplifications 0)
        (println "Lexicase best partial simplification:"
                 (pr-str (not-lazy (:program (auto-simplify lex-best
                                                            error-function
                                                            report-simplifications
                                                            false
                                                            1000))))))
      (when print-errors (println "Lexicase best errors:" (not-lazy (:errors lex-best))))
      (when (and print-errors (not (empty? meta-error-categories)))
        (println "Lexicase best meta-errors:" (not-lazy (:meta-errors lex-best))))
      (println "Lexicase best number of elite cases:" (apply + (map #(if (== %1 %2) 1 0)
                                                                    (:errors lex-best)
                                                                    min-error-by-case)))
      (println "Lexicase best total error:" (:total-error lex-best))
      (println "Lexicase best mean error:" (float (/ (:total-error lex-best)
                                                     (count (:errors lex-best)))))
      (when print-history (println "Lexicase best history:" (not-lazy (:history lex-best))))
      (println "Lexicase best size:" (count-points (:program lex-best)))
      (printf "Percent parens: %.3f\n"
              (double (/ (count-parens (:program lex-best))
                         (count-points (:program lex-best))))) ;Number of (open) parens / points
      (println "--- Lexicase Program with Most Zero Cases Statistics ---")
      (println "Zero cases best genome:" (print-genome most-zero-cases-best argmap))
      (println "Zero cases best program:" (pr-str (not-lazy (:program most-zero-cases-best))))
      (when (> report-simplifications 0)
        (println "Zero cases best partial simplification:"
                 (pr-str (not-lazy (:program (auto-simplify most-zero-cases-best
                                                            error-function
                                                            report-simplifications
                                                            false
                                                            1000))))))
      (when print-errors (println "Zero cases best errors:" (not-lazy (:errors most-zero-cases-best))))
      (when (and print-errors (not (empty? meta-error-categories)))
        (println "Zero cases best meta-errors:" (not-lazy (:meta-errors most-zero-cases-best))))
      (println "Zero cases best number of elite cases:"
               (apply + (map #(if (== %1 %2) 1 0)
                             (:errors most-zero-cases-best)
                             min-error-by-case)))
      (println "Zero cases best number of zero cases:"
               (apply + (map #(if (< %1 min-number-magnitude) 1 0)
                             (:errors most-zero-cases-best))))
      (println "Zero cases best total error:" (:total-error most-zero-cases-best))
      (println "Zero cases best mean error:"
               (float (/ (:total-error most-zero-cases-best)
                         (count (:errors most-zero-cases-best)))))
      (when print-history (println "Zero cases best history:"
                                   (not-lazy (:history most-zero-cases-best))))
      (println "Zero cases best size:" (count-points (:program most-zero-cases-best)))
      (printf "Percent parens: %.3f\n"
              (double (/ (count-parens (:program most-zero-cases-best))
                         (count-points (:program most-zero-cases-best))))) ;Number of (open) parens / points
      )
    (println "--- Lexicase Population Statistics ---")
    (println "Count of elite individuals by case:" count-elites-by-case)
    (println (format "Population mean number of elite cases: %.2f"
                     (float (/ (apply + count-elites-by-case) (count population)))))
    (println "Count of perfect (error zero) individuals by case:" count-zero-by-case)
    (println (format "Population mean number of perfect (error zero) cases: %.2f"
                     (float (/ (apply + count-zero-by-case) (count population)))))))


(defn implicit-fitness-sharing-report
  "This extra report is printed whenever implicit fitness sharing selection is used."
  [population {:keys [print-errors meta-error-categories] :as argmap}]
  (let [ifs-best (apply min-key :weighted-error population)]
    (println "--- Program with Best Implicit Fitness Sharing Error Statistics ---")
    (println "IFS best genome:" (print-genome ifs-best argmap))
    (println "IFS best program:" (pr-str (not-lazy (:program ifs-best))))
    (when print-errors (println "IFS best errors:" (not-lazy (:errors ifs-best))))
    (when (and print-errors (not (empty? meta-error-categories)))
      (println "IFS best meta-errors:" (not-lazy (:meta-errors ifs-best))))
    (println "IFS best total error:" (:total-error ifs-best))
    (println "IFS best mean error:" (float (/ (:total-error ifs-best)
                                              (count (:errors ifs-best)))))
    (println "IFS best IFS error:" (:weighted-error ifs-best))
    (println "IFS best size:" (count-points (:program ifs-best)))
    (printf "IFS best percent parens: %.3f\n"
            (double (/ (count-parens (:program ifs-best))
                       (count-points (:program ifs-best))))))) ;Number of (open) parens / points


(defn report-and-check-for-success
  "Reports on the specified generation of a pushgp run. Returns the best
   individual of the generation."
  [population generation
   {:keys [error-function report-simplifications meta-error-categories
           error-threshold max-generations population-size
           print-errors print-history print-cosmos-data print-timings
           problem-specific-report total-error-method
           parent-selection print-homology-data max-point-evaluations
           max-program-executions use-ALPS
           print-error-frequencies-by-case normalization autoconstructive
           print-selection-counts print-preselection-fraction exit-on-success
           counterexample-driven
           ;; The following are for CSV or JSON logs
           print-csv-logs print-json-logs csv-log-filename json-log-filename
           log-fitnesses-for-all-cases json-log-program-strings
           print-edn-logs edn-keys edn-log-filename edn-additional-keys
           visualize]
    :as argmap}]
  (r/generation-data! [:population]
                      (map #(dissoc % :program) population))

  (println)
  (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (println ";; -*- Report at generation" generation)
  (let [point-evaluations-before-report @point-evaluations-count
        program-executions-before-report @program-executions-count
        err-fn (if (= total-error-method :rmse) :weighted-error :total-error)
        sorted (sort-by err-fn < population)
        err-fn-best (if (not= parent-selection :downsampled-lexicase)
                      (first sorted)
                      ; This tests each individual that passes current generation's subsampled training
                      ; cases on all training cases, and only treats it as winner if it
                      ; passes all of those as well. Otherwise, just returns first individual
                      ; if none pass all subsampled cases, or random individual that passes
                      ; all subsampled cases but not all training cases.
                      (error-function
                       (loop [sorted-individuals sorted]
                         (if (empty? (rest sorted-individuals))
                           (first sorted-individuals)
                           (if (and (<= (:total-error (first sorted-individuals)) error-threshold)
                                    (> (apply + (:errors (error-function (first sorted-individuals) :train))) error-threshold)
                                    (<= (:total-error (second sorted-individuals)) error-threshold))
                             (recur (rest sorted-individuals))
                             (first sorted-individuals))))
                       :train))
        total-error-best (if (not= parent-selection :downsampled-lexicase)
                           err-fn-best
                           (assoc err-fn-best
                                  :total-error
                                  (apply +' (:errors err-fn-best))))
        psr-best (problem-specific-report total-error-best
                                          population
                                          generation
                                          error-function
                                          report-simplifications)
        best (if (= (type psr-best) clojush.individual.individual)
               psr-best
               total-error-best)
        standard-deviation (fn [nums]
                             (if (<= (count nums) 1)
                               (str "Cannot find standard deviation of "
                                    (count nums)
                                    "numbers. Must have at least 2.")
                               (let [mean (mean nums)]
                                 (Math/sqrt (/ (apply +' (map #(* (- % mean) (- % mean))
                                                              nums))
                                               (dec (count nums)))))))
        quartiles (fn [nums]
                    (if (zero? (count nums))
                      "Cannot find quartiles of zero numbers."
                      (let [sorted (sort nums)]
                        (vector (nth sorted
                                     (truncate (/ (count nums) 4)))
                                (nth sorted
                                     (truncate (/ (count nums) 2)))
                                (nth sorted
                                     (truncate (/ (* 3 (count nums)) 4)))))))]

    (when print-error-frequencies-by-case
      (println "Error frequencies by case:"
               (doall (map frequencies (apply map vector (map :errors population))))))
    (when (some #{parent-selection}
                #{:lexicase :elitegroup-lexicase :leaky-lexicase :epsilon-lexicase
                  :random-threshold-lexicase :random-toggle-lexicase
                  :randomly-truncated-lexicase})
          (lexicase-report population argmap))
    (when (= total-error-method :ifs) (implicit-fitness-sharing-report population argmap))
    (println (format "--- Best Program (%s) Statistics ---" (str "based on " (name err-fn))))
    (r/generation-data! [:best :individual] (dissoc best :program))
    (println "Best genome:" (print-genome best argmap))
    (println "Best program:" (pr-str (not-lazy (:program best))))
    (when (> report-simplifications 0)
      (println "Partial simplification:"
               (pr-str (not-lazy (:program (r/generation-data! [:best :individual-simplified]
                                            (auto-simplify best
                                                          error-function
                                                          report-simplifications
                                                          false
                                                          1000)))))))
    (when print-errors (println "Errors:" (not-lazy (:errors best))))
    (when (and print-errors (not (empty? meta-error-categories)))
      (println "Meta-Errors:" (not-lazy (:meta-errors best))))
    (println "Total:" (:total-error best))
    (let [mean (r/generation-data! [:best :mean-error] (float (/ (:total-error best)
                                                                 (count (:errors best)))))]

      (println "Mean:" mean))
    (when (not= normalization :none)
      (println "Normalized error:" (:normalized-error best)))
    (case total-error-method
      :hah (println "HAH-error:" (:weighted-error best))
      :rmse (println "RMS-error:" (:weighted-error best))
      :ifs (println "IFS-error:" (:weighted-error best))
      nil)
    (when (= parent-selection :novelty-search)
      (println "Novelty: " (float (:novelty best))))
    (when print-history (println "History:" (not-lazy (:history best))))
    (println "Genome size:" (r/generation-data! [:best :genome-size] (count (:genome best))))
    (println "Size:" (r/generation-data! [:best :program-size] (count-points (:program best))))
    (printf "Percent parens: %.3f\n"
            (r/generation-data! [:best :percent-parens]
              (double (/ (count-parens (:program best))
                         (count-points (:program best)))))) ;Number of (open) parens / points
    (println "Age:" (:age best))
    (println "--- Population Statistics ---")
    (when print-cosmos-data
      (println "Cosmos Data:" (let [quants (config/quantiles (count population))]
                                (zipmap quants
                                        (map #(:total-error (nth (sort-by :total-error population) %))
                                             quants)))))
    (println "Average total errors in population:"
          (r/generation-data! [:population-report :mean-total-error]
             (*' 1.0 (mean (map :total-error sorted)))))
    (println "Median total errors in population:"
          (r/generation-data! [:population-report :median-total-error]
             (median (map :total-error sorted))))
    (when print-errors (println "Error averages by case:"
                                (apply map (fn [& args] (*' 1.0 (mean args)))
                                       (map :errors population))))
    (when print-errors (println "Error minima by case:"
                                (apply map (fn [& args] (apply min args))
                                       (map :errors population))))
    (when (and print-errors (not (empty? meta-error-categories)))
      (println "Meta-Error averages by category:"
               (apply map (fn [& args] (*' 1.0 (mean args)))
                      (map :meta-errors population)))
      (println "Meta-Error minima by category:"
               (apply map (fn [& args] (apply min args))
                      (map :meta-errors population))))
    (println "Average genome size in population (length):"
          (r/generation-data! [:population-report :mean-genome-size]
             (*' 1.0 (mean (map count (map :genome sorted))))))
    (println "Average program size in population (points):"
          (r/generation-data! [:population-report :mean-program-size]
             (*' 1.0 (mean (map count-points (map :program sorted))))))
    (printf "Average percent parens in population: %.3f\n"
          (r/generation-data! [:population-report :mean-program-percent-params]
            (mean (map #(double (/ (count-parens (:program %)) (count-points (:program %)))) sorted))))
    (let [ages (map :age population)]
      (when use-ALPS
        (println "Population ages:" ages))
      (println "Minimum age in population:"
          (r/generation-data! [:population-report :min-age]
               (* 1.0 (apply min ages))))
      (println "Maximum age in population:"
          (r/generation-data! [:population-report :max-age]
               (* 1.0 (apply max ages))))
      (println "Average age in population:"
          (r/generation-data! [:population-report :mean-age]
               (* 1.0 (mean ages))))
      (println "Median age in population:"
          (r/generation-data! [:population-report :median-age]
               (* 1.0 (median ages)))))
    (let [grain-sizes (map :grain-size population)]
      (println "Minimum grain-size in population:"
          (r/generation-data! [:population-report :min-grain-size]
               (* 1.0 (apply min grain-sizes))))
      (println "Maximum grain-size in population:"
          (r/generation-data! [:population-report :max-grain-size]
               (* 1.0 (apply max grain-sizes))))
      (println "Average grain-size in population:"
          (r/generation-data! [:population-report :mean-grain-size]
               (* 1.0 (mean grain-sizes))))
      (println "Median grain-size in population:"
          (r/generation-data! [:population-report :median-grain-size]
               (* 1.0 (median grain-sizes)))))
    (println "--- Population Diversity Statistics ---")
    (let [genome-frequency-map (frequencies (map :genome population))]
      (println "Min copy number of one genome:"
        (r/generation-data! [:population-report :min-genome-frequency]
          (apply min (vals genome-frequency-map))))
      (println "Median copy number of one genome:"
        (r/generation-data! [:population-report :median-genome-frequency]
          (median (vals genome-frequency-map))))
      (println "Max copy number of one genome:"
        (r/generation-data! [:population-report :max-genome-frequency]
          (apply max (vals genome-frequency-map))))
      (println "Genome diversity (% unique genomes):\t"
        (r/generation-data! [:population-report :percent-genomes-unique]
               (float (/ (count genome-frequency-map) (count population))))))
    (let [frequency-map (frequencies (map :program population))]
      (println "Min copy number of one Push program:"
        (r/generation-data! [:population-report :min-program-frequency]
          (apply min (vals frequency-map))))
      (println "Median copy number of one Push program:"
        (r/generation-data! [:population-report :median-program-frequency]
          (median (vals frequency-map))))
      (println "Max copy number of one Push program:"
        (r/generation-data! [:population-report :max-program-frequency]
          (apply max (vals frequency-map))))
      (println "Syntactic diversity (% unique Push programs):\t"
        (r/generation-data! [:population-report :percent-programs-unique]
               (float (/ (count frequency-map) (count population))))))
    (println "Total error diversity:\t\t\t\t"
        (r/generation-data! [:population-report :percent-total-error-unique]
             (float (/ (count (distinct (map :total-error population))) (count population)))))
    (println "Error (vector) diversity:\t\t\t"
        (r/generation-data! [:population-report :percent-errors-unique]
             (float (/ (count (distinct (map :errors population))) (count population)))))
    (when (not (nil? (:behaviors (first population))))
      (println "Behavioral diversity:\t\t\t\t" (behavioral-diversity population)))
    (when print-homology-data
      (let [num-samples 1000
            sample-1 (sample-population-edit-distance population num-samples)
            [first-quart-1 median-1 third-quart-1] (quartiles sample-1)]
        (println "--- Population Homology Statistics (all stats reference the sampled population edit distance of programs) ---")
        (println "Number of homology samples:" num-samples)
        (println "Average:            " (mean sample-1))
        (println "Standard deviation: " (standard-deviation sample-1))
        (println "First quartile: " first-quart-1)
        (println "Median:         " median-1)
        (println "Third quartile: " third-quart-1)))

    (when print-selection-counts
      (println "Selection counts:"
               (sort > (concat (vals @selection-counts)
                               (repeat (- population-size (count @selection-counts)) 0))))
      (reset! selection-counts {}))
    (when (and print-preselection-fraction
               (not (empty? @preselection-counts)))
      (println "Preselection fraction:" (float (/ (/ (reduce + @preselection-counts) population-size)
                                                  (count @preselection-counts))))
      (reset! preselection-counts []))
    (when autoconstructive
      (println "Number of random replacements for non-diversifying individuals:"
        (r/generation-data! [:population-report :number-random-replacements]
               (count (filter :is-random-replacement population)))))
    (println "--- Run Statistics ---")
    (println "Number of individuals evaluated (running on all training cases counts as 1 evaluation):" @evaluations-count)
    (println "Number of program executions (running on a single case counts as 1 execution):" program-executions-before-report)
    (println "Number of point (instruction) evaluations so far:" point-evaluations-before-report)
    (reset! point-evaluations-count point-evaluations-before-report)
    (reset! program-executions-count program-executions-before-report)

    (println "--- Timings ---")
    (println "Current time:" (System/currentTimeMillis) "milliseconds")
    (when print-timings
      (let [total-time (apply + (vals @timing-map))
            init (get @timing-map :initialization)
            reproduction (get @timing-map :reproduction)
            fitness (get @timing-map :fitness)
            report-time (get @timing-map :report)
            other (get @timing-map :other)]
        (printf "Total Time:      %8.1f seconds\n"
                (/ total-time 1000.0))
        (printf "Initialization:  %8.1f seconds, %4.1f%%\n"
                (/ init 1000.0) (* 100.0 (/ init total-time)))
        (printf "Reproduction:    %8.1f seconds, %4.1f%%\n"
                (/ reproduction 1000.0) (* 100.0 (/ reproduction total-time)))
        (printf "Fitness Testing: %8.1f seconds, %4.1f%%\n"
                (/ fitness 1000.0) (* 100.0 (/ fitness total-time)))
        (printf "Report:          %8.1f seconds, %4.1f%%\n"
                (/ report-time 1000.0) (* 100.0 (/ report-time total-time)))
        (printf "Other:           %8.1f seconds, %4.1f%%\n"
                (/ other 1000.0) (* 100.0 (/ other total-time)))))
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (println ";; -*- End of report for generation" generation)
    (println ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    (flush)
    (when print-csv-logs (csv-print population generation argmap))
    (when print-json-logs (json-print population generation json-log-filename
                                      log-fitnesses-for-all-cases json-log-program-strings))
    (when print-edn-logs
      (edn-print population generation edn-log-filename edn-keys edn-additional-keys))
    ;; Visualization -- update viz-data-atom here
    (when visualize
      (swap! viz-data-atom update-in [:history-of-errors-of-best] conj (:errors best))
      (swap! viz-data-atom assoc :generation generation))
    (let [counterexample-driven-success (if counterexample-driven
                                          (check-counterexample-driven-results sorted argmap)
                                          false)]
      (cond
        ; Succeed for counterexample-driven GP
        counterexample-driven-success
        [:success counterexample-driven-success]
        ; Succeed
        (and exit-on-success
             (not counterexample-driven)
             (or (<= (:total-error best) error-threshold)
                 (:success best)))
        [:success best]
        ; Fail max generations
        (>= generation max-generations)
        [:failure best]
        ; Fail max program executions
        (>= @program-executions-count max-program-executions)
        [:failure best]
        ; Fail max point evaluations
        (>= @point-evaluations-count max-point-evaluations)
        [:failure best]
        ; Continue
        :else
        [:continue best]))))


(defn remove-function-values [argmap]
  (into {} (filter (fn [[k v]] (not (fn? v)))
    (dissoc argmap :random-seed :atom-generators))))

(defn initial-report
  "Prints the initial report of a PushGP run."
  [{:keys [problem-specific-initial-report] :as push-argmap}]
  (problem-specific-initial-report push-argmap)
  (println "Registered instructions:"
    (r/config-data! [:registered-instructions] @registered-instructions))
  (println "Starting PushGP run.")
  (printf "Clojush version = ")
  (try
    (let [version-str (apply str (butlast (re-find #"\".*\""
                                                   (first (string/split-lines
                                                            (local-file/slurp* "project.clj"))))))
          version-number (.substring version-str 1 (count version-str))]
      (if (empty? version-number)
        (throw Exception)
        (printf (str (r/config-data! [:version-number] version-number)) "\n")))
    (flush)
    (catch Exception e
           (printf "version number unavailable\n")
           (flush)))
  (try
    (let [git-hash (git-last-commit-hash)]
      (if (empty? git-hash)
        (throw Exception)
        (do
          ;; NOTES: - Last commit hash will only be correct if this code has
          ;;          been committed already.
          ;;        - GitHub link will only work if commit has been pushed
          ;;          to GitHub.
          (r/config-data! [:git-hash] git-hash)
          (printf (str "Hash of last Git commit = " git-hash "\n"))
          (printf (str "GitHub link = https://github.com/lspector/Clojush/commit/"
                       git-hash
                       "\n"))
          (flush))))
    (catch Exception e
           (printf "Hash of last Git commit = unavailable\n")
           (printf "GitHub link = unavailable\n")
           (flush)))
  (if (:print-edn-logs push-argmap)
    ;; The edn log is overwritten if it exists
    (with-open [w (io/writer (:edn-log-filename push-argmap) :append false)]
      (.write w "#clojush/run")
      (.write w (prn-str (remove-function-values push-argmap)))))
  (when (:visualize push-argmap) ;; Visualization
    ;; Require conditionally (dynamically), avoiding unintended Quil sketch launch
    (require 'clojush.pushgp.visualize)))


(defn final-report
  "Prints the final report of a PushGP run if the run is successful."
  [generation best
   {:keys [error-function final-report-simplifications report-simplifications
           print-ancestors-of-solution problem-specific-report]}]
  (printf "\n\nSUCCESS at generation %s\nSuccessful program: %s\nErrors: %s\nTotal error: %s\nHistory: %s\nSize: %s\n\n"
          generation (pr-str (not-lazy (:program best))) (not-lazy (:errors best)) (:total-error best)
          (not-lazy (:history best)) (count-points (:program best)))
  (when print-ancestors-of-solution
    (printf "\nAncestors of solution:\n")
    (prn (:ancestors best)))
  (let [simplified-best (auto-simplify best error-function final-report-simplifications true 500)]
    (println "\n;;******************************")
    (println ";; Problem-Specific Report of Simplified Solution")
    (problem-specific-report simplified-best [] generation error-function report-simplifications)))
