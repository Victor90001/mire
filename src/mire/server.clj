(ns mire.server
  (:require [clojure.java.io :as io]
            [server.socket :as socket]
            [mire.player :as player]
            [mire.commands :as commands]
            [mire.rooms :as rooms]
            [clojure.string :as str]
            [clojure.java.shell :as shell]))

(defn- cleanup []
  "Drop all inventory and remove player from room and player list."
  (dosync
   (doseq [item @player/*inventory*]
     (commands/discard item))
   (commute player/streams dissoc player/*name*)
   (commute (:inhabitants @player/*current-room*)
            disj player/*name*)))
(defn- get-player-type [name]
  (if (str/includes? name "prologBot")
    "bot"
    "human"))
(defn- get-unique-player-name [name]
  (if (@player/streams name)
    (do (print "That name is in use; try again: ")
        (flush)
        (recur (read-line)))
    name))

(defn- mire-handle-client [in out]
  (binding [*in* (io/reader in)
            *out* (io/writer out)
            *err* (io/writer System/err)]

    ;; We have to nest this in another binding call instead of using
    ;; the one above so *in* and *out* will be bound to the socket
    (print "What is your name?\n") (flush)
    (def x (get-unique-player-name (read-line)))
    (binding [player/*name* x
              player/*player-type* (ref (get-player-type x))
              player/*current-room* (ref (@rooms/rooms :start))
              player/*inventory* (ref #{})
              player/*caught* (ref "")]
      (dosync
       (commute (:inhabitants @player/*current-room*) conj player/*name*)
       (commute player/streams assoc player/*name* *out*)
       (commute player/players-info conj {(keyword player/*name*) {:name player/*name*
                                                                   :type player/*player-type*
                                                                   :caught player/*caught*}}))


      (println (commands/look)) (print player/prompt) (flush)

      (try (loop [input (read-line)]
             (when input
               (println (commands/execute input))
               (.flush *err*)
               (print player/prompt) (flush)
               (recur (read-line))))
           (finally (cleanup))))))

(defn -main
  ([port dir]
   (rooms/add-rooms dir)
   (defonce server (socket/create-server (Integer. port) mire-handle-client))
   (println "Launching Mire server on port" port))
  ([port] (-main port "resources/rooms"))
  ([] (-main 3333)))
