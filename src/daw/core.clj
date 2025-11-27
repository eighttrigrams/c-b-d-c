(ns daw.core
  (:import [javax.sound.sampled AudioSystem Clip]))

(defn load-clip [path]
  (let [clip (AudioSystem/getClip)
        stream (AudioSystem/getAudioInputStream (java.io.File. path))]
    (.open clip stream)
    clip))

(defn play [^Clip clip]
  (.setFramePosition clip 0)
  (.start clip))

(defn -main []
  (let [kick (load-clip "samples/BD Kick 006 HC.wav")
        snare (load-clip "samples/SN Sd 4Bit Vinyl St GB.wav")
        hh (load-clip "samples/HH 60S Stomp2 GB.wav")
        interval 400]
    (println "Playing 4/4 (Ctrl+C to stop)")
    (loop [beat 0]
      (play hh)
      (case (mod beat 4)
        0 (play kick)
        2 (play snare)
        nil)
      (Thread/sleep interval)
      (recur (inc beat)))))
