(ns daw.server
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :refer [response content-type]]
            [compojure.core :refer [defroutes GET POST PUT DELETE]]
            [compojure.route :as route]
            [clojure.string :as str]
            [daw.core :as core]
            [daw.ai :as ai]
            [daw.db :as db])
  (:import [java.io File]
           [javax.sound.sampled AudioSystem AudioFileFormat$Type]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defroutes app-routes
  (GET "/" []
    (content-type (response (slurp "resources/public/index.html")) "text/html"))
  (GET "/api/state" []
    (response @core/state))
  (GET "/api/tracks" []
    (response @core/sequence'))
  (GET "/api/sequences" []
    (response (core/list-sequences)))
  (PUT "/api/sequence" {body :body}
    (let [filename (:filename body)]
      (reset! core/sequence' (core/load-sequence filename))
      (response {:loaded filename})))
  (PUT "/api/master" {body :body}
    (swap! core/state assoc :master (:value body))
    (response @core/state))
  (PUT "/api/mixer/:ch" [ch :as {body :body}]
    (let [ch-idx (Integer/parseInt ch)]
      (swap! core/state update :mixer assoc ch-idx (:value body)))
    (response @core/state))
  (PUT "/api/bpm" {body :body}
    (swap! core/state assoc :bpm (:value body))
    (response @core/state))
  (PUT "/api/playing" {body :body}
    (swap! core/state assoc :playing (:value body))
    (response @core/state))
  (PUT "/api/next-bar" {body :body}
    (swap! core/state assoc :next-bar (:value body))
    (response @core/state))
  (PUT "/api/selected-bars" {body :body}
    (reset! core/selected-bars (vec (:value body)))
    (response {:selected-bars @core/selected-bars}))
  (POST "/api/chat" {body :body}
    (let [api-key (ai/load-api-key)
          text (:text body)]
      (if (nil? api-key)
        (response {:error "No API key. Set ANTHROPIC_API_KEY or create .anthropic-api-key"})
        (response {:reply (ai/chat api-key text)}))))
  (POST "/api/chat/reset" []
    (reset! ai/history [])
    (response {:reset true}))
  (GET "/api/projects" []
    (response (db/list-projects)))
  (POST "/api/projects" {body :body}
    (let [name (:name body)]
      (if (str/blank? name)
        {:status 400 :body {:error "name required"}}
        (do (db/save-project {:name name
                              :sequence @core/sequence'
                              :mixer (:mixer @core/state)
                              :master (:master @core/state)
                              :bpm (:bpm @core/state)})
            (response (db/list-projects))))))
  (PUT "/api/projects/:id" [id]
    (if-let [p (db/get-project (Integer/parseInt id))]
      (do (reset! core/sequence' (:sequence p))
          (swap! core/state assoc
                 :mixer (:mixer p)
                 :master (:master p)
                 :bpm (:bpm p))
          (response {:loaded (:name p)}))
      {:status 404 :body {:error "not found"}}))
  (DELETE "/api/projects/:id" [id]
    (db/delete-project (Integer/parseInt id))
    (response (db/list-projects)))
  (GET "/api/export" []
    (let [export-dir (File. ".exports")
          _ (when-not (.exists export-dir) (.mkdirs export-dir))
          timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss"))
          filename (str "export-" timestamp ".wav")
          file (File. export-dir filename)
          samples (core/load-track-samples)
          n-steps (* (core/sequence-length-bars) 16)
          audio-bytes (core/render-steps samples n-steps)
          stream (javax.sound.sampled.AudioInputStream.
                  (java.io.ByteArrayInputStream. audio-bytes)
                  core/output-format
                  (/ (alength audio-bytes) 4))]
      (AudioSystem/write stream AudioFileFormat$Type/WAVE file)
      (response {:exported (.getPath file)})))
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      (wrap-json-body {:keywords? true})
      wrap-json-response
      (wrap-resource "public")))

(defn start []
  (db/init-db)
  (println "Starting server on http://localhost:3015")
  (run-jetty app {:port 3015 :join? false}))

(defn -main []
  (start)
  @(promise))
