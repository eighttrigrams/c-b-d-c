(ns daw.server
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :refer [response content-type]]
            [compojure.core :refer [defroutes GET POST PUT]]
            [compojure.route :as route]
            [daw.core :as core])
  (:import [java.io File]
           [javax.sound.sampled AudioSystem AudioFileFormat$Type]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defroutes app-routes
  (GET "/" []
    (content-type (response (slurp "resources/public/index.html")) "text/html"))
  (GET "/api/state" []
    (response @core/state))
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
  (GET "/api/export" []
    (let [export-dir (File. ".exports")
          _ (when-not (.exists export-dir) (.mkdirs export-dir))
          timestamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyy-MM-dd_HH-mm-ss"))
          filename (str "export-" timestamp ".wav")
          file (File. export-dir filename)
          samples {:kick  (core/sample->stereo-ints (core/load-sample "samples/BD Kick 006 HC.wav"))
                   :snare (core/sample->stereo-ints (core/load-sample "samples/SN Sd 4Bit Vinyl St GB.wav"))
                   :hh    (core/sample->stereo-ints (core/load-sample "samples/HH 60S Stomp2 GB.wav"))
                   :reso  (core/sample->stereo-ints (core/load-sample "samples/reso.wav"))}
          n-steps (* 8 16)
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
  (println "Starting server on http://localhost:3015")
  (run-jetty app {:port 3015 :join? false}))

(defn -main []
  (start)
  @(promise))
