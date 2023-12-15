(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

;; Command functions

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*)
       "\nExits: " (keys @(:exits @player/*current-room*)) "\n"
       (str/join (map #(str "There is " % " here.\n")
                      @(:items @player/*current-room*)))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)
         (look))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
         (str "You picked up the " thing "."))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (println player/prompt)))
    (str "You said " message)))
(defn whisper
  "Whisper something to anyone in the room."
  [name & words]
  (let [message (str/join " " words)]
    (let [inhabitant (get @(:inhabitants @player/*current-room*) name)]
      (binding [*out* (player/streams inhabitant)]
        (println message)
        (print player/prompt)))
    (str "You said " message)))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))
(defn player-type
  "Shows your current player type."
  []
  (str "Your player type:\n" @player/*player-type*))

(defn stats
  "Show player statistics"
  ([] (str
       "Name: " player/*name*
       "\nPlayer type: " @player/*player-type*
       (if (= @player/*player-type* "bot") (str "\nCatched: " @player/*caught*) "")))
  ([name]
   (if (contains? (disj @(:inhabitants @player/*current-room*) player/*name*) name)
     (if-let [player1 (first (filter #(= (:name %) name)
                                     (vals @player/players-info)))]
       (str "Name: " (:name player1)
            "\nPlayer type: " @(:type player1)
            (if (= @(:type player1) "bot") (str "\nCatched: " @(:caught player1)) "")))
     (str ""))))

(defn catch
  "Find a bot and stun him 4ever."
  [name]
  (if (and
       (= @player/*player-type* "human")
       (contains? (disj @(:inhabitants @player/*current-room*) player/*name*) name))
    (if-let [bot (first (filter #(= @(:type %) "bot") (vals @player/players-info)))]
      (whisper (:name bot) "You got caught.")
      "This is not a bot.")
    "You can't do that! Maybe you're the bot or there is no bot in current room."))

(defn players
  "Show players in the room"
  []
  (str/join "\n" (map stats @(:inhabitants @player/*current-room*))))
  ;; (str "\n" (seq @(:inhabitants @player/*current-room*)))


(defn lobby
  "Show players in the labyrinth"
  []
  (str @player/streams))
  ;; (str "\n" (seq @(:inhabitants @player/*current-room*)))

;; Command data

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "players" players
               "stats" stats
               "lobby" lobby
               "catch" catch
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "whisper" whisper
               "help" help})

;; Command handling

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))
