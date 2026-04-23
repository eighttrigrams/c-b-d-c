(ns daw.db
  (:require [next.jdbc :as jdbc]
            [next.jdbc.sql :as sql]
            [clojure.edn :as edn])
  (:import [java.io File]))

(def data-dir "data")

(defn ensure-data-dir []
  (let [d (File. data-dir)]
    (when-not (.exists d) (.mkdirs d))))

(defn ds []
  (ensure-data-dir)
  (jdbc/get-datasource {:dbtype "sqlite" :dbname (str data-dir "/daw.db")}))

(defn init-db []
  (jdbc/execute! (ds)
    ["CREATE TABLE IF NOT EXISTS projects (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        name TEXT NOT NULL UNIQUE,
        sequence TEXT NOT NULL,
        mixer TEXT NOT NULL,
        master INTEGER NOT NULL,
        bpm INTEGER NOT NULL,
        updated_at TEXT NOT NULL DEFAULT (datetime('now')))"]))

(defn list-projects []
  (->> (sql/query (ds) ["SELECT id, name, updated_at FROM projects ORDER BY name"])
       (map (fn [r] {:id (:projects/id r)
                     :name (:projects/name r)
                     :updated-at (:projects/updated_at r)}))))

(defn get-project [id]
  (when-let [r (first (sql/query (ds) ["SELECT * FROM projects WHERE id = ?" id]))]
    {:id (:projects/id r)
     :name (:projects/name r)
     :sequence (edn/read-string (:projects/sequence r))
     :mixer (edn/read-string (:projects/mixer r))
     :master (:projects/master r)
     :bpm (:projects/bpm r)}))

(defn save-project [{:keys [name sequence mixer master bpm]}]
  (jdbc/execute! (ds)
    ["INSERT INTO projects (name, sequence, mixer, master, bpm)
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(name) DO UPDATE SET
          sequence=excluded.sequence,
          mixer=excluded.mixer,
          master=excluded.master,
          bpm=excluded.bpm,
          updated_at=datetime('now')"
     name (pr-str sequence) (pr-str mixer) master bpm])
  (first (sql/query (ds) ["SELECT id, name FROM projects WHERE name = ?" name])))

(defn delete-project [id]
  (jdbc/execute! (ds) ["DELETE FROM projects WHERE id = ?" id]))
