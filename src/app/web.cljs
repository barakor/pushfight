(ns app.web
  (:require
   [reagent.core :as r :refer [with-let]]
   [reagent.dom :as rdom]
   [rewig.components :refer [box row column gap button]]
   [pushfight.core :as pf]
   [rewig.theme.gruvbox :as theme]
   [clojure.string :as string]))

(defn cell->box [cell & {:keys [cell-background]}]
  (let [piece (:piece cell)
        anchored? (pf/anchored? cell)
        icon-color (cond
                     ; (and (some? piece) anchored?) "#cc241d"
                     (pf/black? piece) theme/bg0
                     (pf/white? piece) theme/fg0

                     anchored?     theme/light-orange
                     :transparent  "rgba(0,0,0,0)")

        icon       (cond
                     (pf/wall-cell? cell) "󰟾"
                     anchored?            ""
                     (pf/round? piece)    ""
                     (pf/pusher? piece)   ""
                     :empty-cell          "")
        icon-background-color (when (and anchored? (some? piece)) theme/danger)
        background-color (cond
                           cell-background      cell-background
                           (pf/void-cell? cell) "rgba(0,0,0,0)"
                           (pf/wall-cell? cell) theme/danger
                           :else                theme/light-gray)
        border-color (cond
                       cell-background theme/primary
                       
                       :else           "#00000000")]

    [box {:css {:background-color background-color
                :border (str "2px solid " border-color)
                :color icon-color}
          :padding 3}
     [row {:css {:background-color icon-background-color
                 :cursor "pointer"}}
      icon]]))

(defn display-board [board cell-background? cell-click!]
  [column
   (for [rn (range (count board))
         :let [r (get board rn)]]
     [row
      [(doall
        (for [cn (range (count r))
              :let [cell (get r cn)
                    cell-key (string/join "-" ["cell" rn cn])]]

          ^{:key cell-key} [box {:padding 0.2
                                 :click! #(cell-click! cell [rn cn])}
                            [cell->box cell
                             :cell-background (cell-background? [rn cn])]]))]])])

(defn game [board* game-over!]
  (with-let [selected-cell* (r/atom nil)
             move-to-cells* (r/atom #{})
             pushable-cells* (r/atom #{})
             turn-count*     (r/atom 1)
             team-turn*      (r/atom :white)
             remaining-moves* (r/atom 2)

             game-log* (r/atom [{:board @board*
                                 :turn-count @turn-count*
                                 :team-turn @team-turn*
                                 :remaining-moves @remaining-moves*}])

             highlight-cells! (fn [pos]
                                (when (> @remaining-moves* 0) (reset! move-to-cells* (pf/get-available-move-pos @board* pos)))
                                (reset! pushable-cells* (pf/get-available-push-pos @board* pos))
                                (reset! selected-cell* pos))
             clear-selection! (fn []
                                (reset! move-to-cells* nil)
                                (reset! pushable-cells* nil)
                                (reset! selected-cell* nil))]

    (let [board @board*
          selected-cell @selected-cell*
          move-to-cells @move-to-cells*
          pushable-cells @pushable-cells*
          turn-count @turn-count*
          team-turn @team-turn*
          remaining-moves @remaining-moves*
          game-log @game-log*
          undo! (fn []
                  (swap! game-log* #(into [] (drop-last %)))
                  (let [{board :board
                         turn-count :turn-count
                         team-turn :team-turn
                         remaining-moves :remaining-moves} (last @game-log*)]
                    (reset! board* board)
                    (reset! turn-count* turn-count)
                    (reset! team-turn* team-turn)
                    (reset! remaining-moves* remaining-moves)))
          update-game-state! (fn []
                               (swap! game-log* conj {:board @board*
                                                      :turn-count @turn-count*
                                                      :team-turn @team-turn*
                                                      :remaining-moves @remaining-moves*}))
          move-piece!     (fn [src dest]
                            (swap! board* pf/move-piece src dest)
                            (swap! remaining-moves* dec)
                            (update-game-state!)
                            (clear-selection!))
          push-piece!     (fn [src dest]
                            (swap! board* pf/remove-anchors)
                            (swap! board* pf/push-piece src dest)
                            (swap! board* pf/anchor-cell dest)
                            (clear-selection!)
                            (swap! turn-count* inc)
                            (swap! team-turn* #(if (= % :white) :black :white))
                            (reset! remaining-moves* 2)
                            (update-game-state!)
                            (when (pf/game-over? board)
                              (game-over!)))
          cell-background? (fn [pos]
                             (cond
                               (contains? move-to-cells pos)  theme/light-green
                               (contains? pushable-cells pos) theme/danger
                               (= selected-cell pos)          theme/highlight))
          box-click! (fn [cell pos]
                       (cond
                         (and (not (:piece cell))
                              (not (contains? move-to-cells pos))
                              (not (contains? pushable-cells pos))) (clear-selection!)

                         (and (:piece cell)
                              (not (contains? move-to-cells pos))
                              (not (contains? pushable-cells pos))
                              (= @team-turn* (pf/team? (:piece cell)))) (highlight-cells! pos)

                         (and (not (nil? selected-cell))
                              (contains? move-to-cells pos))        (move-piece! selected-cell pos)

                         (and (not (nil? selected-cell))
                              (contains? pushable-cells pos))       (push-piece! selected-cell pos)))]

      (println game-log)
      [column
       [[row {:css {:color theme/text}}
         (str "Turn #" turn-count ": " (name team-turn) " turn")]
        (when (> remaining-moves 0)
          [row {:css {:color theme/text}}
           (str remaining-moves " Moves left")])
        [row {:css {:color theme/text}} "Push to finish the turn"]
        [gap :size 20]
        [display-board board cell-background? box-click!]
        [gap :size 20]
        [row
         [[gap :size 60]
          [button {; :css {:background-color theme/primary}
                      ;       :color theme/text}
                   :type :danger
                   :disabled? (<= (count game-log) 1)
                      ; :align :center :content-align :center
                   :click! undo!}
           "Undo!"]]]]])))

(defn place-pieces [board* start-game!]
  (with-let [unplaced-pieces* (r/atom (into []
                                            (concat
                                             (repeat 3 pf/white-square)
                                             (repeat 2 pf/white-round)
                                             (repeat 2 pf/black-round)
                                             (repeat 3 pf/black-square)))) ;; replace with stack (would also look better...) 
             selected-cell*   (r/atom nil)
             move-to-cells*   (r/atom #{})
             clear-selection! (fn []
                                (reset! move-to-cells* nil)
                                (reset! selected-cell* nil))
             highlight-cells! (fn [piece pos]
                                (let [side (cond
                                             (pf/black? piece) :right
                                             (pf/white? piece) :left)]
                                  (reset! move-to-cells* (pf/get-half-board-pos @board* side))
                                  (reset! selected-cell* pos)))
             move-piece!      (fn [src dest]
                                (cond
                                  (number? src) (do (swap! board* update-in dest pf/place-piece (nth @unplaced-pieces* src))
                                                    (swap! unplaced-pieces* assoc src nil)
                                                    (clear-selection!))
                                  :else         (do (swap! board* pf/move-piece src dest)
                                                    (clear-selection!))))]

    (let [board @board*
          unplaced-pieces @unplaced-pieces*
          selected-cell @selected-cell*
          move-to-cells @move-to-cells*

          cell-background? (fn [pos] (cond
                                       (contains? move-to-cells pos)  theme/light-green
                                       (= selected-cell pos)          theme/highlight))
          piece-click! (fn [piece pos]
                         (cond
                           (number? pos) (highlight-cells! piece pos)
                           (and (not piece)
                                (not (contains? move-to-cells pos))) (clear-selection!)))

          cell-click! (fn [{piece :piece} pos]
                        (cond
                          (and (not piece)
                               (not (contains? move-to-cells pos))) (clear-selection!)

                          (and piece
                               (not (contains? move-to-cells pos))) (highlight-cells! piece pos)

                          (and (not (nil? selected-cell))
                               (contains? move-to-cells pos))       (move-piece! selected-cell pos)))]
      [column
       [[box {:css {:color theme/text}} "pieces to choose from: "]
        [row
         (for [idx (range (count unplaced-pieces))
               :let [piece (nth unplaced-pieces idx)]]
           ^{:key idx} [box {:padding 0.2
                             :click! #(piece-click! piece idx)}
                        [cell->box (assoc pf/floor-cell :piece piece)
                         :cell-background (cell-background? idx)]])]
        [gap :size 10]
        [display-board board cell-background? cell-click!]
        [gap :size 10]
        [row
         [[gap :size 60]
          [button {; :css {:background-color theme/primary}
                      ;       :color theme/text}
                   :type :danger
                   :disabled? (not-empty (remove nil? unplaced-pieces))
                      ; :align :center :content-align :center
                   :click! start-game!}
           "Start Game!"]]]]])))

(defn start-menu [board* initial-board* start-game! restart-game! default-game! place-pieces!]
  (let [board @board*
        initial-board @initial-board*]
    [column
     [[gap :size 10]
      [button {:type :danger
               :click! place-pieces!}
       "Place Pieces"]
      [gap :size 10]
      (when @initial-board*
        [button {:type :danger
                 :click! restart-game!}
         "Reuse Arrangement"])
      (when @initial-board*
        [gap :size 10])
      [button {:type :danger
               :click! default-game!}
       "Default Arrangement"]
      [gap :size 10]]]))

(defn app []
  (with-let [game-lifecycle-stage* (r/atom :game)
             board* (r/atom pf/sample-board)
             initial-board* (r/atom nil)

             start-game! #(do (reset! initial-board* @board*)
                              (reset! game-lifecycle-stage* :game))

             restart-game! #(do (reset! board* @initial-board*)
                                (reset! game-lifecycle-stage* :game))

             default-game! #(do (reset! board* pf/sample-board)
                                (start-game!))

             place-pieces! #(do (reset! board* (pf/make-standard-board))
                                (reset! game-lifecycle-stage* :placement))

             game-over!    #(reset! game-lifecycle-stage* :start-menu)]

    (let [k (atom 0)]
      [box {:css {:background-color theme/background
                  :height "100%"}
            :size "100%"}
       [[gap :size "35%"]
        (case @game-lifecycle-stage*
          :start-menu (start-menu board* initial-board* start-game! restart-game! default-game! place-pieces!)
          :placement (place-pieces board* start-game!)
          :game (game board* game-over!))]])))

(defn ^:export main []
  (rdom/render [app]
               (.getElementById js/document "app")))

(main)
