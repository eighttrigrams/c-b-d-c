(ns daw.ui
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))

(defonce state (r/atom {:master 127
                        :mixer [127 127 50 110]
                        :bpm 120
                        :playing true}))

(def channel-names ["Kick" "Snare" "HiHat" "Reso"])

(defn fetch-state []
  (-> (js/fetch "/api/state")
      (.then #(.json %))
      (.then #(reset! state (js->clj % :keywordize-keys true)))))

(defn set-master [value]
  (-> (js/fetch "/api/master"
                #js {:method "PUT"
                     :headers #js {"Content-Type" "application/json"}
                     :body (js/JSON.stringify #js {:value (js/parseInt value)})})
      (.then #(swap! state assoc :master (js/parseInt value)))))

(defn set-channel [ch value]
  (-> (js/fetch (str "/api/mixer/" ch)
                #js {:method "PUT"
                     :headers #js {"Content-Type" "application/json"}
                     :body (js/JSON.stringify #js {:value (js/parseInt value)})})
      (.then #(swap! state assoc-in [:mixer ch] (js/parseInt value)))))

(defn set-bpm [value]
  (-> (js/fetch "/api/bpm"
                #js {:method "PUT"
                     :headers #js {"Content-Type" "application/json"}
                     :body (js/JSON.stringify #js {:value value})})
      (.then #(swap! state assoc :bpm value))))

(defn set-playing [value]
  (-> (js/fetch "/api/playing"
                #js {:method "PUT"
                     :headers #js {"Content-Type" "application/json"}
                     :body (js/JSON.stringify #js {:value value})})
      (.then #(swap! state assoc :playing value))))

(defn slider [{:keys [label value on-change]}]
  [:div.channel
   [:label label]
   [:input {:type "range"
            :min 0
            :max 127
            :value value
            :on-change #(on-change (.. % -target -value))}]
   [:span.value value]])

(defonce export-status (r/atom nil))

(defn export-wav []
  (reset! export-status "Exporting...")
  (-> (js/fetch "/api/export")
      (.then #(.json %))
      (.then (fn [result]
               (reset! export-status (str "Exported: " (.-exported result)))
               (js/setTimeout #(reset! export-status nil) 3000)))))

(defn transport []
  [:div.transport
   [:button.play-btn
    {:on-click #(set-playing (not (:playing @state)))}
    (if (:playing @state) "⏹" "▶")]
   (when-not (:playing @state)
     [:button.export-btn {:on-click export-wav} "Export WAV"])
   (when @export-status
     [:span.export-status @export-status])
   [:div.tempo
    [:button.tempo-btn {:on-click #(set-bpm (dec (:bpm @state)))} "◀"]
    [:span.tempo-display (:bpm @state)]
    [:button.tempo-btn {:on-click #(set-bpm (inc (:bpm @state)))} "▶"]]])

(defn mixer-ui []
  [:div.mixer
   [slider {:label "Master"
            :value (:master @state)
            :on-change set-master}]
   [:hr]
   (doall
    (for [[idx vol] (map-indexed vector (:mixer @state))]
      ^{:key idx}
      [slider {:label (nth channel-names idx)
               :value vol
               :on-change #(set-channel idx %)}]))])

(defn app []
  [:div
   [:h1 "AI DAW"]
   [transport]
   [mixer-ui]])

(defn init []
  (fetch-state)
  (rdom/render [app] (.getElementById js/document "app")))
