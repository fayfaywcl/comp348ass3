(ns weather
  (:require [clojure.string :as str]))

(declare main-menu handle-choice)

(defn parse-line
  "Parses a comma-separated line into a weather report map."
  [line]
  (let [[date location temp condition] (str/split line #",")]
    {:date date
     :location location
     :temperature (Integer/parseInt temp)
     :condition condition
     :unit "°C"})) ;; Added default unit as required

(defn load-reports
  "Loads weather reports from a file.
  Please refer to https://clojure.org/guides/threading_macros for more details on ->>
  "
  [filename]
  (if (.exists (java.io.File. filename))
    (->> (slurp filename)
         str/split-lines
         (map parse-line)
         vec)
    []))

;; ===== PURE HELPER FUNCTIONS =====

(defn celsius-to-fahrenheit
  "Pure function: Converts Celsius to Fahrenheit."
  [celsius]
   (Math/round (+ (* celsius (/ 9.0 5.0)) 32))) ; Round to nearest integer

(defn fahrenheit-to-celsius
  "Pure function: Converts Fahrenheit to Celsius."
  [fahrenheit]
   (Math/round (* (- fahrenheit 32) (/ 5.0 9.0)))) ; Round to nearest integer

(defn convert-temperature-unit
  "Pure function: Converts temperature based on current unit."
  [report target-unit]
  (let [current-unit (:unit report)
        temp (:temperature report)]
    (cond
      (= current-unit target-unit) report
      (and (= current-unit "°C") (= target-unit "°F"))
      (assoc report
             :temperature (int (celsius-to-fahrenheit temp))
             :unit target-unit)
      (and (= current-unit "°F") (= target-unit "°C"))
      (assoc report
             :temperature (int (fahrenheit-to-celsius temp))
             :unit target-unit)
      :else report)))

(defn format-temperature
  "Pure function: Formats temperature with unit."
  [report]
  (str (:temperature report) (:unit report)))

(defn calculate-average
  "Pure function: Calculates average of a collection of numbers."
  [numbers]
  (if (empty? numbers)
    0
    (/ (reduce + numbers) (count numbers))))

(defn get-temperatures
  "Pure function: Extracts temperatures from reports."
  [reports]
  (map :temperature reports))

(defn find-extreme-reports
  "Pure function: Finds hottest and coldest reports."
  [reports]
  (if (empty? reports)
    {:hottest nil :coldest nil}
    {:hottest (apply max-key :temperature reports)
     :coldest (apply min-key :temperature reports)}))

(defn get-unique-conditions
  "Pure function: Gets sorted unique conditions."
  [reports]
  (->> reports
       (map :condition)
       set
       sort))

(defn filter-by-condition-pred
  "Pure function: Creates predicate for filtering by condition."
  [target-condition]
  #(= (:condition %) target-condition))

(defn filter-by-temperature-range-pred
  "Pure function: Creates predicate for filtering by temperature range."
  [min-temp max-temp]
  #(and (>= (:temperature %) min-temp)
        (<= (:temperature %) max-temp)))

(defn get-dominant-unit
  "Pure function: Gets the most common unit from reports."
  [reports]
  (if (empty? reports)
    "°C"
    (->> reports
         (map :unit)
         frequencies
         (apply max-key val)
         key)))

(defn compute-statistics
  "Pure function: Computes all statistics and returns data structure."
  [reports]
  (if (empty? reports)
    {:empty true}
    (let [temperatures (get-temperatures reports)
          avg-temp (calculate-average temperatures)
          extremes (find-extreme-reports reports)
          unique-conditions (get-unique-conditions reports)
          dominant-unit (get-dominant-unit reports)]
      {:empty false
       :average-temperature avg-temp
       :hottest-report (:hottest extremes)
       :coldest-report (:coldest extremes)
       :unique-conditions unique-conditions
       :condition-count (count unique-conditions)
       :dominant-unit dominant-unit})))

(defn format-statistics-display
  "Pure function: Formats statistics data into display strings."
  [stats]
  (if (:empty stats)
    ["\nNo weather reports available for statistics."]
    (let [avg-line (format "Average Temperature: %.2f%s"
                           (double (:average-temperature stats))
                           (:dominant-unit stats))
          hottest (:hottest-report stats)
          coldest (:coldest-report stats)
          hottest-line (format "Hottest day: %-10s | %-16s | %-11s | %-11s"
                               (:date hottest) (:location hottest)
                               (format-temperature hottest) (:condition hottest))
          coldest-line (format "Coldest day: %-10s | %-16s | %-11s | %-11s"
                               (:date coldest) (:location coldest)
                               (format-temperature coldest) (:condition coldest))
          conditions-line (format "Unique Weather Conditions: %d" (:condition-count stats))
          conditions-list (format "Conditions: %s" (str/join ", " (:unique-conditions stats)))]
      ["\n=== Weather Statistics ==="
       avg-line
       ""
       hottest-line
       coldest-line
       ""
       conditions-line
       conditions-list])))

;; ===== I/O HELPER FUNCTIONS =====

(defn get-user-input
  "I/O function: Gets user input with prompt."
  [prompt]
  (print prompt)
  (flush)
  (read-line))

; for filter-weather-reports[]
(defn valid-integer? 
  "Check if the input can be parsed as an integer."
  [input]
  (try
    (Integer/parseInt input)
    true
    (catch NumberFormatException _ false)))

(defn display-error-message 
  "Display error message for invalid input."
  []
  (println "Invalid input. Please enter a valid integer."))

(defn prompt-for-integer
  "Prompt for an integer, re-prompting if the input is invalid."
  [prompt]
  (let [input (get-user-input prompt)]
    (if (valid-integer? input)
      (Integer/parseInt input)  ; Return parsed integer if valid
      (do
        (display-error-message)  ; Call the side-effect function
        (prompt-for-integer prompt)))))  ; Recursive call if invalid input

(defn display-table-header
  "I/O function: Displays table header."
  []
  (println "\n+------------+------------------+-------------+-------------+")
  (println "| Date       | Location         | Temperature | Condition   |")
  (println "+------------+------------------+-------------+-------------+"))

(defn display-table-footer
  "I/O function: Displays table footer."
  []
  (println "+------------+------------------+-------------+-------------+"))

(defn display-report-row
  "I/O function: Displays a single report row."
  [report]
  (printf "| %-10s | %-16s | %-11s | %-11s |\n"
          (:date report)
          (:location report)
          (format-temperature report)
          (:condition report)))

; for view-weather-reports []
(defn print-no-reports-available
  "I/O function: Print a message indicating no weather reports are available."
  []
  (println "\nNo weather reports available."))

(defn print-total-reports
  "I/O function: Print the total number of weather reports."
  [reports]
  (println (str "\nTotal Weather Reports: " (count reports))))

; for filter-weather-reports[]

(defn print-no-reports-to-filter
  "Print message when there are no reports available to filter."
  []
  (println "\nNo weather reports available to filter."))

(defn print-filter-options
  "Print the filter options to the user."
  []
  (println "\nFilter Options:")
  (println "1. Filter by Condition")
  (println "2. Filter by Temperature Range"))

(defn print-no-reports-for-condition
  "Print message when no reports are found for a specific condition."
  [condition]
  (println (str "\nNo reports found for condition: " condition)))

(defn print-filtered-reports-condition
  "Print filtered reports for a specific weather condition."
  [condition]
  (println (str "\nFiltered reports for condition: " condition)))

(defn print-no-reports-for-temperature-range
  "Print message when no reports are found in a temperature range."
  [min-temp max-temp dominant-unit]
  (println (str "\nNo reports found in temperature range (" dominant-unit "): " min-temp " to " max-temp)))

(defn print-filtered-reports-temperature-range
  "Print filtered reports for a temperature range."
  [min-temp max-temp dominant-unit]
  (println (str "\nFiltered reports for temperature range (" dominant-unit "): " min-temp " to " max-temp)))

(defn print-invalid-choice
  "Print message when the user enters an invalid choice."
  []
  (println "Invalid choice."))

; transform-weather-reports []
(defn print-no-reports-to-transform
  "Print message when there are no reports available to transform."
  []
  (println "\nNo weather reports available to transform."))

(defn print-transformation-options
  "Print the transformation options to the user."
  []
  (println "\nTransformation Options:")
  (println "1. Convert Celsius to Fahrenheit")
  (println "2. Convert Fahrenheit to Celsius"))

(defn print-transformation-success
  "Print success message after temperature transformation."
  [unit]
  (println (str "\nTemperatures converted to " unit ".")))

(defn print-invalid-transformation-choice
  "Print message when the user enters an invalid transformation choice."
  []
  (println "Invalid choice. No transformation applied."))

; weather-statistics[]
(defn print-weather-statistics
  "Print the formatted weather statistics."
  [display-lines]
  (doseq [line display-lines]
    (println line)))

;; ===== MAIN IMPLEMENTATION FUNCTIONS =====

(defn save-reports
  "Write your code. Saving is optional"
  [reports])

;; (defn save-reports
;;   "Write reports to a file."
;;   [reports]
;;   (with-open [writer (clojure.java.io/writer "weather_data.txt")]
;;     (doseq [report reports]
;;       (.write writer (str (:date report) "," (:location report) "," (:temperature report) "," (:condition report) "\n")))))

(defn view-weather-reports
  "Write code to display weather reports in a tabular format."
  [reports]
  (if (empty? reports)
    (print-no-reports-available)
    (let [sorted-reports (sort-by :date reports)]
      (print-total-reports reports)
      (display-table-header)
      (doseq [report sorted-reports]
        (display-report-row report))
      (display-table-footer))))

(defn filter-weather-reports
  "Write code to filter reports based on condition or temperature range."
  [reports]
  (if (empty? reports)
    (print-no-reports-to-filter)
    (let [dominant-unit (get-dominant-unit reports)]
      (print-filter-options)
      (let [choice (get-user-input "Enter your choice (1-2): ")]
        (case choice
          "1" (let [condition (get-user-input "Enter weather condition: ")
                    filtered (filter (filter-by-condition-pred condition) reports)]
                (if (empty? filtered)
                  (print-no-reports-for-condition condition)
                  (do
                    (print-filtered-reports-condition condition)
                    (view-weather-reports filtered))))
          "2" (let [min-temp (prompt-for-integer (str "Enter minimum temperature(" dominant-unit "): ")); Ensure valid integer input
                    max-temp (prompt-for-integer (str "Enter maximum temperature(" dominant-unit "): "))
                    filtered (filter (filter-by-temperature-range-pred min-temp max-temp) reports)]
                (if (empty? filtered)
                  (print-no-reports-for-temperature-range min-temp max-temp dominant-unit)
                  (do
                    (print-filtered-reports-temperature-range min-temp max-temp dominant-unit)
                    (view-weather-reports filtered))))
          (print-invalid-choice))))))

(defn transform-weather-reports
  "Write code to apply user-selected transformation to the weather report collection."
  [reports]
  (if (empty? reports)
    (print-no-reports-to-transform)
    (do
      (print-transformation-options)
      (let [choice (get-user-input "Enter your choice (1-2): ")]
        (case choice
          "1" (let [transformed (map #(convert-temperature-unit % "°F") reports)]
                (print-transformation-success "Fahrenheit")
                (view-weather-reports transformed)
                transformed)
          "2" (let [transformed (map #(convert-temperature-unit % "°C") reports)]
                (print-transformation-success "Celsius")
                (view-weather-reports transformed)
                transformed)
          (do
            (print-invalid-transformation-choice)
            reports))))))

(defn weather-statistics
  "Calculate and display weather statistics: average, hottest, coldest, unique conditions."
  [reports]
  (if (empty? reports)
    (print-no-reports-available)
    (let [stats (compute-statistics reports)
          display-lines (format-statistics-display stats)]
      (print-weather-statistics display-lines))))

(defn exit-program []
  (println "\nThank you for using the Weather Report System. Goodbye!")
  (System/exit 0))

(defn main-menu
  ([file]
   (main-menu file (load-reports file)))
  ([file reports]
   (println "\n=== Weather Report System ===")
   (println "1. View Weather Reports")
   (println "2. Transform Weather Report")
   (println "3. Filter Weather Reports")
   (println "4. Weather Statistics")
   (println "5. Save and Exit")
   (print "Enter your choice (1-5): ")
   (flush)
   (let [choice (read-line)]
     (handle-choice choice reports file))))

(defn handle-choice [choice reports file]
  (case choice
    "1" (do (view-weather-reports reports)
            (main-menu file reports))
    "2" (let [updated (transform-weather-reports reports)]
          (main-menu file updated))
    "3" (do (filter-weather-reports reports)
            (main-menu file reports))
    "4" (do (weather-statistics reports)
            (main-menu file reports))
    "5" (exit-program)
    (do (println "Invalid option. Try again.")
        (main-menu file reports))))

(defn display-welcome-message 
  "Display welcome message " 
  []
  (println "Welcome to the Weather Report System!"))

;; Entry point
(defn -main [& args]
  (display-welcome-message) ; display welcome message
  (let [file "weather_data.txt"]
    (main-menu file)))

(-main)