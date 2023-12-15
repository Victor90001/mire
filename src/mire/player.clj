(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *player-type*)
(def ^:dynamic *inventory*)
(def ^:dynamic *caught*)
(def ^:dynamic *name*)

(def prompt "> ")
(def streams (ref {}))
(def players-info (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))
