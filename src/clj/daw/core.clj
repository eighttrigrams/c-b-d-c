(ns daw.core
  (:require [clojure.edn :as edn])
  (:import [javax.sound.sampled AudioSystem AudioFormat SourceDataLine DataLine$Info AudioFormat$Encoding AudioFileFormat$Type]
           [java.io ByteArrayInputStream File]))

(def sample-rate 44100.0)

(defonce state (atom {:master 127
                       :mixer [127 127 50 110 127 127 127 127]
                       :bpm 120
                       :playing true
                       :step 0}))

(defn frames-per-16th []
  (int (/ (* sample-rate 60) (:bpm @state) 4)))

(defn load-sequence [filename]
  (edn/read-string (slurp (str "sequences/" filename))))

(defn list-sequences []
  (->> (file-seq (File. "sequences"))
       (filter #(.isFile %))
       (filter #(.endsWith (.getName %) ".edn"))
       (map #(.getName %))
       sort))

(defonce sequence' (atom (load-sequence "basic.edn")))

(defn sequence-length-bars []
  (apply max 0 (map #(count (:bars %)) @sequence')))

(def output-format (AudioFormat. sample-rate 16 2 true false))

(defn load-sample [path]
  (let [stream (AudioSystem/getAudioInputStream (java.io.File. path))
        src-format (.getFormat stream)
        target-format (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                    (.getSampleRate src-format)
                                    16
                                    (.getChannels src-format)
                                    (* 2 (.getChannels src-format))
                                    (.getSampleRate src-format)
                                    false)
        converted (AudioSystem/getAudioInputStream target-format stream)
        bytes (byte-array (.available converted))]
    (.read converted bytes)
    (.close converted)
    {:bytes bytes :channels (.getChannels src-format)}))

(defn sample->stereo-ints [{:keys [bytes channels]}]
  (let [n-samples (/ (alength bytes) 2)
        n-frames (/ n-samples channels)
        out (int-array (* 2 n-frames))]
    (dotimes [i n-frames]
      (let [left (+ (bit-and (aget bytes (* i channels 2)) 0xFF)
                    (bit-shift-left (aget bytes (+ (* i channels 2) 1)) 8))]
        (aset out (* 2 i) left)
        (aset out (+ (* 2 i) 1) (if (= channels 2)
                                   (+ (bit-and (aget bytes (+ (* i channels 2) 2)) 0xFF)
                                      (bit-shift-left (aget bytes (+ (* i channels 2) 3)) 8))
                                   left))))
    out))

(defn ints->bytes [^ints arr]
  (let [out (byte-array (* 2 (alength arr)))]
    (dotimes [i (alength arr)]
      (let [v (aget arr i)]
        (aset out (* 2 i) (unchecked-byte (bit-and v 0xFF)))
        (aset out (+ (* 2 i) 1) (unchecked-byte (bit-shift-right v 8)))))
    out))

(defn mix-sample
  ([^ints buffer ^ints sample velocity]
   (mix-sample buffer sample velocity 0))
  ([^ints buffer ^ints sample velocity offset]
   (let [len (min (- (alength sample) offset) (alength buffer))
         gain (/ velocity 127.0)]
     (when (pos? len)
       (dotimes [i len]
         (let [scaled (int (* (aget sample (+ offset i)) gain))
               mixed (+ (aget buffer i) scaled)]
           (aset buffer i (int (max -32768 (min 32767 mixed))))))
       (+ offset len)))))

(defn render-steps [samples n-steps]
  (let [samples-per-16th (* (frames-per-16th) 2)]
    (loop [step 0
           voices []
           output []]
      (if (>= step n-steps)
        (let [all-bytes (byte-array (mapcat seq output))]
          all-bytes)
        (let [buffer (int-array samples-per-16th)
              bar-idx (quot step 16)
              step-idx (mod step 16)
              new-voices (doall
                          (for [[ch {:keys [sample bars]}] (map-indexed vector @sequence')
                                :let [bar-data (get bars bar-idx)
                                      velocity (if bar-data (nth bar-data step-idx) 0)
                                      channel-vol (nth (:mixer @state) ch)
                                      master-vol (:master @state)]
                                :when (pos? velocity)]
                            {:sample sample :velocity (* (/ velocity 127.0) channel-vol (/ master-vol 127.0)) :offset 0}))
              triggered-samples (set (map :sample new-voices))
              continuing-voices (remove #(triggered-samples (:sample %)) voices)
              all-voices (concat continuing-voices new-voices)
              remaining (doall
                         (for [{:keys [sample velocity offset]} all-voices
                               :let [new-offset (mix-sample buffer (get samples sample) velocity offset)]
                               :when (and new-offset
                                          (< new-offset (alength (get samples sample))))]
                           {:sample sample :velocity velocity :offset new-offset}))]
          (recur (inc step) remaining (conj output (ints->bytes buffer))))))))

(defn load-track-samples []
  (->> @sequence'
       (filter :sample)
       (map (fn [{:keys [sample]}] [sample (sample->stereo-ints (load-sample sample))]))
       (into {})))

(defn export-wav [{:keys [filename] :or {filename "output.wav"}}]
  (let [samples (load-track-samples)
        n-steps (* 8 16)
        audio-bytes (render-steps samples n-steps)
        stream (javax.sound.sampled.AudioInputStream.
                (ByteArrayInputStream. audio-bytes)
                output-format
                (/ (alength audio-bytes) 4))]
    (AudioSystem/write stream AudioFileFormat$Type/WAVE (File. filename))
    (println (str "Exported " filename))))

(defn start-server []
  (require 'daw.server)
  (future ((resolve 'daw.server/start))))

(defn -main []
  (start-server)
  (let [info (DataLine$Info. SourceDataLine output-format)
        line (AudioSystem/getLine info)
        samples (load-track-samples)]
    (.open ^SourceDataLine line output-format)
    (.start ^SourceDataLine line)
    (println "Playing 4/4 (Ctrl+C to stop)\nMixer UI: http://localhost:3015")
    (loop [step 0
           voices []]
      (let [samples-per-16th (* (frames-per-16th) 2)
            total-steps (* 16 (max 1 (sequence-length-bars)))]
        (if (:playing @state)
          (let [buffer (int-array samples-per-16th)
                looped-step (mod step total-steps)
                bar-idx (quot looped-step 16)
                step-idx (mod looped-step 16)
                _ (swap! state assoc :step looped-step)
                new-voices (doall
                            (for [[ch {:keys [sample bars]}] (map-indexed vector @sequence')
                                  :let [bar-data (get bars bar-idx)
                                        velocity (if bar-data (nth bar-data step-idx) 0)
                                        channel-vol (nth (:mixer @state) ch)
                                        master-vol (:master @state)]
                                  :when (pos? velocity)]
                              {:sample sample :velocity (* (/ velocity 127.0) channel-vol (/ master-vol 127.0)) :offset 0}))
                triggered-samples (set (map :sample new-voices))
                continuing-voices (remove #(triggered-samples (:sample %)) voices)
                all-voices (concat continuing-voices new-voices)
                remaining (doall
                           (for [{:keys [sample velocity offset]} all-voices
                                 :let [new-offset (mix-sample buffer (get samples sample) velocity offset)]
                                 :when (and new-offset
                                            (< new-offset (alength (get samples sample))))]
                             {:sample sample :velocity velocity :offset new-offset}))]
            (let [out (ints->bytes buffer)]
              (.write ^SourceDataLine line out 0 (alength out)))
            (recur (inc step) remaining))
          (do
            (Thread/sleep 50)
            (recur 0 [])))))))
