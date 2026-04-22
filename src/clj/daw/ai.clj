(ns daw.ai
  (:require [daw.tools :as tools]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]))

(def ^:private client (HttpClient/newHttpClient))
(def ^:private model "claude-haiku-4-5-20251001")
(def ^:private max-tool-iterations 5)

(def ^:private system-prompt
  (str "You are the AI co-pilot for a drum-machine/DAW. "
       "The user describes changes to the beat in plain language; you use tools to edit the sequence. "
       "Bars are 0-indexed. 'First bar' = 0, 'second bar' = 1, etc. "
       "Call get_sequence if you need to inspect current state. "
       "Reply briefly after applying changes."))

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
  (loop [msgs [{:role "user" :content user-text}]
         iter 0]
    (if (>= iter max-tool-iterations)
      "(tool-use iteration limit reached)"
      (let [resp (post-messages api-key
                                {:model model
                                 :max_tokens 1024
                                 :system system-prompt
                                 :tools tools/tool-specs
                                 :messages msgs})
            content (:content resp)
            _ (println "Claude response:" (pr-str resp))]
        (if (= "tool_use" (:stop_reason resp))
          (let [results (mapv run-tool-use (tool-uses content))]
            (recur (conj msgs
                         {:role "assistant" :content content}
                         {:role "user" :content results})
                   (inc iter)))
          (extract-text content))))))
