(ns daw.tools
  (:require [daw.core :as core]))

(def tool-specs
  [{:name "copy_bar"
    :description (str "Copy a bar to another position across all tracks in the sequence. "
                      "Bars are 0-indexed. If to_bar is beyond the current length, tracks are "
                      "padded with empty bars. Overwrites whatever was at to_bar.")
    :input_schema {:type "object"
                   :properties {:from_bar {:type "integer"
                                           :description "Source bar index (0-based)"}
                                :to_bar {:type "integer"
                                         :description "Destination bar index (0-based)"}}
                   :required ["from_bar" "to_bar"]}}
   {:name "get_sequence"
    :description "Return the current sequence as EDN: a vector of tracks with :sample and :bars."
    :input_schema {:type "object" :properties {}}}])

(defn- empty-bar [] (vec (repeat 16 0)))

(defn- copy-bar-in-track [{:keys [bars] :as track} from-bar to-bar]
  (if (< from-bar (count bars))
    (let [src (nth bars from-bar)
          padded (if (<= to-bar (count bars))
                   bars
                   (into bars (repeat (- to-bar (count bars)) (empty-bar))))]
      (assoc track :bars (assoc padded to-bar src)))
    track))

(defn- copy-bar [{:keys [from_bar to_bar]}]
  (swap! core/sequence' (fn [s] (mapv #(copy-bar-in-track % from_bar to_bar) s)))
  (str "Copied bar " from_bar " to bar " to_bar "."))

(defn- get-sequence [_]
  (pr-str @core/sequence'))

(defn run [{:keys [name input]}]
  (case name
    "copy_bar" (copy-bar input)
    "get_sequence" (get-sequence input)
    (str "Unknown tool: " name)))
