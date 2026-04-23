(ns daw.ai
  (:require [daw.tools :as tools]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]))

(def ^:private client (HttpClient/newHttpClient))
(def ^:private model "claude-sonnet-4-6")
(def ^:private max-tool-iterations 5)
(def ^:private max-turns 15)

(defonce history (atom []))

(defn- new-user-turn? [{:keys [role content]}]
  (and (= role "user") (string? content)))

(defn- trim-history [msgs]
  (let [user-idx (keep-indexed #(when (new-user-turn? %2) %1) msgs)
        n (count user-idx)]
    (if (<= n max-turns)
      (vec msgs)
      (vec (drop (nth user-idx (- n max-turns)) msgs)))))

(def ^:private system-prompt
  (str "You are C.B.D.C., the AI co-pilot for a drum-machine/DAW. "
       "The user describes changes to the beat in plain language; you apply them by editing the sequence. "
       "\n\nWorkflow: "
       "1) Call read_sequence to fetch the current state. "
       "2) Compute the desired new sequence. "
       "3) Call write_sequence with the full updated array. "
       "\n\nTerminology: "
       "A bar has 16 steps (1-16) and 4 beats (1-4). Beat N = step (N-1)*4 + 1. "
       "Bars are 1-indexed when the user says 'first bar', 'second bar', etc. "
       "\n\nPreserve the sample field of tracks you aren't changing. "
       "After writing, reply briefly (one sentence) describing what you did."))

(defn load-api-key []
  (or (when-let [v (System/getenv "ANTHROPIC_API_KEY")]
        (when-not (str/blank? v) v))
      (let [f (io/file ".anthropic-api-key")]
        (when (.exists f)
          (let [s (str/trim (slurp f))]
            (when-not (str/blank? s) s))))))

(defn- post-messages [api-key body]
  (let [req (-> (HttpRequest/newBuilder)
                (.uri (URI/create "https://api.anthropic.com/v1/messages"))
                (.header "Content-Type" "application/json")
                (.header "x-api-key" api-key)
                (.header "anthropic-version" "2023-06-01")
                (.POST (HttpRequest$BodyPublishers/ofString (json/write-str body)))
                (.build))
        resp (.send client req (HttpResponse$BodyHandlers/ofString))]
    (json/read-str (.body resp) :key-fn keyword)))

(defn- extract-text [content]
  (->> content (filter #(= "text" (:type %))) (map :text) (apply str)))

(defn- tool-uses [content]
  (filter #(= "tool_use" (:type %)) content))

(defn- run-tool-use [{:keys [id name input]}]
  (try
    (let [result (tools/run {:name name :input input})]
      (println "Tool invoked:" name "input:" input)
      {:type "tool_result" :tool_use_id id :content result})
    (catch Exception e
      {:type "tool_result" :tool_use_id id :is_error true
       :content (str "Error: " (.getMessage e))})))

(defn chat [api-key user-text]
  (loop [msgs (conj @history {:role "user" :content user-text})
         iter 0]
    (if (>= iter max-tool-iterations)
      (do (reset! history (trim-history msgs))
          "(tool-use iteration limit reached)")
      (let [resp (post-messages api-key
                                {:model model
                                 :max_tokens 4096
                                 :system system-prompt
                                 :tools tools/tool-specs
                                 :messages msgs})
            content (:content resp)]
        (cond
          (or (nil? content) (not (sequential? content)))
          (do (println "Claude API error, history preserved:" (pr-str resp))
              (str "API error: " (or (get-in resp [:error :message]) (pr-str resp))))

          (= "tool_use" (:stop_reason resp))
          (let [results (mapv run-tool-use (tool-uses content))]
            (recur (conj msgs
                         {:role "assistant" :content content}
                         {:role "user" :content results})
                   (inc iter)))

          :else
          (do (reset! history (trim-history (conj msgs {:role "assistant" :content content})))
              (extract-text content)))))))
