;; Define the namespace and require necessary Clojure libraries and Java classes.
(ns hello-world.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.io File])
  (:import [java.util.regex Pattern]))

;; Define a regular expression pattern for validating date strings.
(def date-regex-pattern (re-pattern "^\\w+\\s+\\d{1,2},\\s+\\d{4}$"))

;; Function to validate date strings against the date regex pattern.
(defn valid-date? [date]
  (re-matches date-regex-pattern date))

;; Function to read a date string from the console and validate it.
(defn read-valid-date []
  (let [date (read-line)]
    (if (valid-date? date)
      date
      (do (println "Invalid date format. Please use 'Month DD, YYYY'.")
          (recur)))))

;; Define a regular expression pattern for validating location strings.
(def location-regex-pattern (re-pattern "^[\\w\\s,]+$"))

;; Function to validate location strings against the location regex pattern.
(defn valid-location? [location]
  (re-matches location-regex-pattern location))

;; Function to read a location string from the console and validate it.
(defn read-valid-location []
  (let [location (read-line)]
    (if (valid-location? location)
      location
      (do (println "Invalid location format. Please enter only letters, commas, and spaces.")
          (recur)))))

;; Function to read eclipse events from a file. And parse them into a vector of maps.
(defn read-eclipse-events [file-path]
  (with-open [rdr (io/reader file-path)]
    (let [lines (doall (line-seq rdr))
          events (partition 5 lines)]
      (vec (map (fn [block]
                  (zipmap [:date :location :type :significance] block))
                events)))))

;; Function to display eclipse events.
(defn display-eclipse-events [events]
  (println "\nTotal events found:" (count events))
  (println "")
  (doseq [event events]
    (println (:date event))
    (println (:location event))
    (println (:type event))
    (println (:significance event))
    (println "----------------------------------------------------------------------------------------")))

;; Function to add eclipse events.
(defn add-eclipse-event [file-path]
  (print "Enter date:") (flush)
  (let [date (str "Date: " (read-valid-date) "\n")
        location (str "Location: " (do (print "Enter location:") (flush) (read-valid-location)) "\n")
        type (str "Type: " (do (print "Enter type:") (flush) (read-line)) "\n")
        significance (str "Significance: " (do (print "Enter significance:") (flush) (read-line)) "\n\n")]
    (spit file-path (str date location type significance) :append true)
    (println "Event added successfully.")))

;; Function to list all eclipse events with index numbers for reference.
(defn list-eclipse-events [events]
  (println "")
  (doseq [[index event] (map-indexed vector events)]
    (println (str "Event Index: " (inc index)))
    (println (:date event))
    (println (:location event))
    (println (:type event))
    (println (:significance event))
    (println "----------------------------------------------------------------------------------------")))

;; Function to update an existing eclipse event entry based on user input
(defn update-eclipse-event [events index]
  (println (str "\nModifying Event at Index: " index))
  (let [index (dec index)
        event-to-update (nth events index)
        ;; get-value
        ;; A helper function to extract the value from a string with the format "key: value".
        get-value (fn [s] (second (str/split s #":\s?")))
        
        current-date (get-value (:date event-to-update))
        current-location (get-value (:location event-to-update))
        current-type (get-value (:type event-to-update))
        current-significance (get-value (:significance event-to-update))

        new-date (do (print (str "Enter updated date [" current-date "]: ")) (flush) (read-valid-date))
        new-location (do (print (str "Enter updated location [" current-location "]: ")) (flush) (read-valid-location))
        new-type (do (print (str "Enter updated type [" current-type "]: ")) (flush) (read-line))
        new-significance (do (print (str "Enter updated significance [" current-significance "]: ")) (flush) (read-line))

        ;;assoc is a function in Clojure that is used to associate a new value with a key in a map or a value with an index in a vector. 
        updated-event (assoc event-to-update
                             :date (if (empty? new-date) (:date event-to-update) (str "Date: " new-date))
                             :location (if (empty? new-location) (:location event-to-update) (str "Location: " new-location))
                             :type (if (empty? new-type) (:type event-to-update) (str "Type: " new-type))
                             :significance (if (empty? new-significance) (:significance event-to-update) (str "Significance: " new-significance)))]
    (println "Event updated successfully.")
    (assoc events index updated-event)))

;; Function to handle the user interface for modifying an eclipse event.
(defn modify-eclipse-event [file-path events]
  (list-eclipse-events events)
  (let [max-index (count events)]
    (loop [index (do (println (str "\nEnter the index of the event you wish to modify (1-" max-index "): ")) (flush)
                     (try
                       (Integer/parseInt (read-line))
                       (catch NumberFormatException e 0)))]  ; Default to 0 on format error
      (if (and (>= index 1) (<= index max-index))
        (let [updated-events (update-eclipse-event events index)]
          (with-open [writer (io/writer file-path)]
            (doseq [event updated-events]
              (.write writer (str (:date event) "\n" (:location event) "\n" (:type event) "\n" (:significance event) "\n\n"))))
          updated-events)
        (do (println (str "Invalid index. Please enter a number between 1 and " max-index "."))
            (recur (do (println (str "\nEnter the index of the event you wish to modify (1-" max-index "): ")) (flush)
                       (try
                         (Integer/parseInt (read-line))
                         (catch NumberFormatException e 0)))))))))

;; Function to search eclipse events based on user-specified criteria.
(defn search-eclipse-events [events]
  (let [valid-search-types #{"date" "location"}]
    (loop [search-type (do (print "Enter search type (date/location): ") (flush)
                           (str/lower-case (read-line)))]
      (if (contains? valid-search-types search-type)
        (let [query-prompt (if (= "date" search-type) "Enter search query for date: " "Enter search query for location: ")
              search-field (if (= "date" search-type) :date :location)]
          (print query-prompt) (flush)
          (let [search-query (str/lower-case (read-line))
                regex-pattern (re-pattern (str ".*" (Pattern/quote search-query) ".*"))
                matching-events (filter (fn [event]
                                          (re-find regex-pattern
                                                   (str/lower-case (event search-field))))
                                        events)]
            (if (empty? matching-events)
              (println "No matching events found.")
              (display-eclipse-events matching-events))))
        (do (println "Invalid search type. Please enter 'date' or 'location'.")
            (recur (do (print "Enter search type (date/location): ") (flush)
                       (str/lower-case (read-line)))))))))

;; Function to display the main menu and prompt the user for a choice.
(defn display-menu []
  (println "\n=== Eclipse History Encyclopedia ===")
  (println "1. View Eclipse Events")
  (println "2. Add New Eclipse Event")
  (println "3. Modify Eclipse Event")
  (println "4. Search for Eclipse Events")
  (println "5. Exit")
  (print "\nEnter your choice (1-5): ") (flush))

;; Function to start the main loop of the program
(defn main-loop [file-path]
  (let [events (atom (read-eclipse-events file-path))]
    (loop []
      (display-menu)
      (let [choice (Integer/parseInt (read-line))]
        (cond
          (= choice 1) (display-eclipse-events @events)
          (= choice 2) (do (add-eclipse-event file-path)
                           (reset! events (read-eclipse-events file-path)))
          (= choice 3) (do (modify-eclipse-event file-path @events)
                           (reset! events (read-eclipse-events file-path)))
          (= choice 4) (search-eclipse-events @events)
          (= choice 5) (do (println "Exiting...") (System/exit 0))
          :else (println "Invalid choice, please enter a number from 1 to 5")))
      (recur))))

;; Main function to start the program
(defn -main [& args]
  (let [file-path (if (empty? args) "eclipse-events.txt" (first args))]
    (println "Starting Eclipse History Encyclopedia...")
    (main-loop file-path)))

;; Invoke the main function
(-main)