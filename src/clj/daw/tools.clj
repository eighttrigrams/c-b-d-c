(ns daw.tools
  (:require [daw.core :as core]
            [clojure.data.json :as json]))

(def tool-specs
  [{:name "get_selection"
    :description (str "Return the currently selected/visible bars (1-indexed) that the user is viewing "
                      "and editing in the grid. Up to 4 bars can be selected. "
                      "Use this when the user says 'this bar', 'the visible bars', 'these bars', etc.")
    :input_schema {:type "object" :properties {}}}
   {:name "read_sequence"
    :description (str "Return the entire current sequence as JSON. "
                      "Shape: an array of tracks, each {\"sample\": string|null, \"bars\": [bar, ...]}. "
                      "A bar is an array of notes; each note is {\"step\": 1-16, \"velocity\": 0-127}. "
                      "Missing steps are silent. Tracks with sample=null are empty slots.")
    :input_schema {:type "object" :properties {}}}
   {:name "write_sequence"
    :description (str "Replace the entire sequence. Same shape as read_sequence returns. "
                      "A bar has 16 steps (1-16) and 4 beats (1-4). Beat N = step (N-1)*4 + 1, "
                      "i.e. beat 1=step 1, beat 2=step 5, beat 3=step 9, beat 4=step 13. "
                      "Steps between beats are 16th-note subdivisions. "
                      "Provide only the steps that should sound; omit silent steps. "
                      "Preserve the sample field of tracks you don't want to change.")
    :input_schema {:type "object"
                   :properties {:sequence {:type "array"
                                           :description "Full sequence: array of tracks."
                                           :items {:type "object"
                                                   :properties {:sample {:type ["string" "null"]}
                                                                :bars {:type "array"
                                                                       :items {:type "array"
                                                                               :items {:type "object"
                                                                                       :properties {:step {:type "integer"}
                                                                                                    :velocity {:type "integer"}}
                                                                                       :required ["step" "velocity"]}}}}
                                                   :required ["sample" "bars"]}}}
                   :required ["sequence"]}}])

(defn- bar->notes [bar]
  (->> bar
       (map-indexed (fn [i v] (when (pos? v) {:step (inc i) :velocity v})))
       (filter some?)
       vec))

(defn- notes->bar [notes]
  (reduce (fn [bar {:keys [step velocity]}]
            (if (and step (<= 1 step 16))
              (assoc bar (dec step) (int velocity))
              bar))
          (vec (repeat 16 0))
          notes))

(defn- track-out [{:keys [sample bars]}]
  {:sample sample :bars (mapv bar->notes bars)})

(defn- track-in [{:keys [sample bars]}]
  {:sample sample :bars (mapv notes->bar bars)})

(defn- get-selection [_]
  (json/write-str {:selected_bars (mapv inc @core/selected-bars)}))

(defn- read-sequence [_]
  (json/write-str (mapv track-out @core/sequence')))

(defn- write-sequence [{:keys [sequence]}]
  (reset! core/sequence' (mapv track-in sequence))
  (str "Wrote sequence: " (count sequence) " tracks, "
       (apply max 0 (map #(count (:bars %)) sequence)) " bars."))

(defn run [{:keys [name input]}]
  (case name
    "get_selection" (get-selection input)
    "read_sequence" (read-sequence input)
    "write_sequence" (write-sequence input)
    (str "Unknown tool: " name)))
