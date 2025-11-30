(ns daw.ui
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))

(defonce state (r/atom {:master 127
                        :mixer [127 127 50 110]
                        :bpm 120
                        :playing true
                        :step 0}))

(defonce active-tab (r/atom :sequencer))

(defonce playhead (r/atom {:step 0 :last-sync 0 :sync-step 0}))

(def channel-names ["Kick" "Snare" "HiHat" "Reso" "" "" "" ""])

(def pattern
  [{:sample :kick  :steps [127 0 0 0 127 0 0 0 127 0 0 0 127 0 0 0]}
   {:sample :snare :steps [0 0 0 0 127 0 0 0 0 0 0 0 127 0 0 0]}
   {:sample :hh    :steps [127 0 80 0 127 0 80 0 127 0 80 0 127 0 80 0]}
   {:sample :reso  :steps [127 0 20 0 127 0 0 0 0 0 0 0 0 0 0 0]}
   {:sample nil    :steps [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]}
   {:sample nil    :steps [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]}
   {:sample nil    :steps [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]}
   {:sample nil    :steps [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]}])

(defn fetch-state []
  (-> (js/fetch "/api/state")
      (.then #(.json %))
      (.then #(reset! state (js->clj % :keywordize-keys true)))))

(defn sync-playhead []
  (-> (js/fetch "/api/state")
      (.then #(.json %))
      (.then (fn [result]
               (let [data (js->clj result :keywordize-keys true)]
                 (reset! state data)
                 (reset! playhead {:step (:step data)
                                   :last-sync (.now js/Date)
                                   :sync-step (:step data)}))))))

(defn calc-current-step []
  (if (:playing @state)
    (let [bpm (:bpm @state)
          ms-per-step (/ 60000 bpm 4)
          elapsed (- (.now js/Date) (:last-sync @playhead))
          steps-elapsed (js/Math.floor (/ elapsed ms-per-step))]
      (mod (+ (:sync-step @playhead) steps-elapsed) 16))
    0))

(defonce animation-frame (atom nil))
(defonce sync-interval (atom nil))

(defn start-playhead-loop []
  (letfn [(tick []
            (swap! playhead assoc :step (calc-current-step))
            (reset! animation-frame (js/requestAnimationFrame tick)))]
    (tick)))

(defn stop-playhead-loop []
  (when @animation-frame
    (js/cancelAnimationFrame @animation-frame)
    (reset! animation-frame nil)))

(defn start-sync []
  (sync-playhead)
  (reset! sync-interval (js/setInterval sync-playhead 2000)))

(defn stop-sync []
  (when @sync-interval
    (js/clearInterval @sync-interval)
    (reset! sync-interval nil)))

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
      (.then (fn []
               (swap! state assoc :playing value)
               (if value
                 (do (start-sync) (start-playhead-loop))
                 (do (stop-sync) (stop-playhead-loop) (swap! playhead assoc :step 0)))))))

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

(defn drum-grid []
  (let [current-step (:step @playhead)
        visible-bars 4
        steps-per-bar 16]
    [:div.grid-container
     [:div.grid
      (doall
       (for [[row-idx {:keys [steps]}] (map-indexed vector pattern)]
         ^{:key row-idx}
         [:div.grid-row
          [:span.row-label (nth channel-names row-idx)]
          (doall
           (for [bar (range visible-bars)
                 col (range steps-per-bar)
                 :let [col-idx (+ (* bar steps-per-bar) col)
                       vel (nth steps col)
                       ghost? (pos? bar)
                       bar-end? (= col (dec steps-per-bar))]]
             ^{:key col-idx}
             [:div.grid-cell {:class [(when (pos? vel) "active")
                                      (when ghost? "ghost")
                                      (when bar-end? "bar-end")]
                              :style {:opacity (if ghost?
                                                 0.15
                                                 (if (pos? vel) (/ vel 127) 0.15))}}]))]))]
     [:div.playhead {:style {:left (str (+ 60 (* current-step 18)) "px")}}]]))

(defn transport []
  [:div.transport
   [:button.play-btn
    {:on-click #(set-playing (not (:playing @state)))}
    (if (:playing @state) "⏹" "▶")]
   [:div.tempo
    [:button.tempo-btn {:on-click #(set-bpm (dec (:bpm @state)))} "◀"]
    [:span.tempo-display (:bpm @state)]
    [:button.tempo-btn {:on-click #(set-bpm (inc (:bpm @state)))} "▶"]]])

(defn vertical-fader [{:keys [label value on-change]}]
  [:div.vfader
   [:input {:type "range"
            :min 0
            :max 127
            :value value
            :orient "vertical"
            :on-change #(on-change (.. % -target -value))}]
   [:span.vfader-value value]
   [:label label]])

(defn mixer-ui []
  [:div.mixer
   (doall
    (for [[idx vol] (map-indexed vector (:mixer @state))]
      ^{:key idx}
      [vertical-fader {:label (nth channel-names idx)
                       :value vol
                       :on-change #(set-channel idx %)}]))
   [:div.vfader.master-fader
    [:input {:type "range"
             :min 0
             :max 127
             :value (:master @state)
             :orient "vertical"
             :on-change #(set-master (.. % -target -value))}]
    [:span.vfader-value (:master @state)]
    [:label "Master"]]])

(defn tabs []
  [:div.tabs
   [:button.tab {:class (when (= @active-tab :sequencer) "active")
                 :on-click #(reset! active-tab :sequencer)} "Sequencer"]
   [:button.tab {:class (when (= @active-tab :mixer) "active")
                 :on-click #(reset! active-tab :mixer)} "Mixer"]
   [:button.tab {:class (when (= @active-tab :settings) "active")
                 :on-click #(reset! active-tab :settings)} "Settings"]])

(defn settings-ui []
  [:div.settings
   [:h2 "Export"]
   [:button.export-btn {:on-click export-wav} "Export WAV"]
   (when @export-status
     [:span.export-status @export-status])])

(defn app []
  [:div
   [tabs]
   [transport]
   (case @active-tab
     :sequencer [drum-grid]
     :mixer [mixer-ui]
     :settings [settings-ui])])

(defn init []
  (fetch-state)
  (start-sync)
  (start-playhead-loop)
  (rdom/render [app] (.getElementById js/document "app")))
