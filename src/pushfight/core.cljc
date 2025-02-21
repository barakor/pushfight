(ns pushfight.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.set :as set]
            [clojure.string :as string]))

(def piece-options {:type #{:square :round}
                    :team #{:black :white}})

(def cell-options {:type #{:void :wall :floor}
                   :piece #{{:keys [:type :team]} nil}
                   :anchored? #{true false}})

(defrecord Cell [type
                 piece
                 anchored?])


(def floor-cell (map->Cell {:type :floor
                            :piece nil
                            :anchored? false}))

(def wall-cell {:type :wall
                :anchored? true})

(def void-cell {:type :void})

(def square-piece {:type :square})

(def round-piece {:type :round})

(def black-square (assoc square-piece :team :black))

(def black-round  (assoc round-piece :team :black))

(def white-square (assoc square-piece :team :white))

(def white-round  (assoc round-piece :team :white))

(defn black? [piece]
  (= :black (:team piece)))

(defn white? [piece]
  (= :white (:team piece)))

(defn team? [piece]
  (:team piece))

(defn pusher? [{type :type}]
  (= type (:type square-piece)))

(defn round? [{type :type}]
  (= type (:type round-piece)))

(defn void-cell? [{type :type}]
  (= type (:type void-cell)))

(defn wall-cell? [{type :type}]
  (= type (:type wall-cell)))

(defn floor-cell? [{type :type}]
  (= type (:type floor-cell)))

(defn open-cell? [cell]
  (and
   (= (:type cell) (:type floor-cell))
   (nil? (:piece cell))))

(defn anchored? [cell]
  (or (wall-cell? cell) (:anchored? cell) false))

;; 4x8 board 
;; ⬛ ⬛ 🔲 🔲 🔲 🔲 🔲 ⬛ 
;; 🔲 🔲 🔲 🔲 🔲 🔲 🔲 🔲
;; 🔲 🔲 🔲 🔲 🔲 🔲 🔲 🔲
;; ⬛ 🔲 🔲 🔲 🔲 🔲 ⬛ ⬛
(defn make-standard-board []
  ;;         0           1           2           3           4           5           6           7           8           9
  (vec [[void-cell   void-cell   void-cell   wall-cell   wall-cell   wall-cell   wall-cell   wall-cell   void-cell   void-cell]    ;; 0
        [void-cell   void-cell   void-cell   floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  void-cell   void-cell]    ;; 1
        [void-cell   floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  void-cell]    ;; 2
        [void-cell   floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  void-cell]    ;; 3
        [void-cell   void-cell   floor-cell  floor-cell  floor-cell  floor-cell  floor-cell  void-cell   void-cell   void-cell]    ;; 4
        [void-cell   void-cell   wall-cell   wall-cell   wall-cell   wall-cell   wall-cell   void-cell   void-cell   void-cell]])) ;; 5

(defn get-half-board-pos [board side]
  (let [op (case side
             :left <
             :right >=)

        pos (for [rn (range (count board))
                  cn (range (count (get board rn)))
                  :let [cell (get-in board [rn cn])
                        half-pos (/ (count (get board rn)) 2)]]
              (when (and (open-cell? cell) (op cn half-pos))
                [rn cn]))]
    (->> pos
         (remove nil?)
         (set))))

(defn place-piece [cell piece]
  (assoc cell :piece piece))

(defn anchor-cell [board pos]
  (update-in board pos #(assoc % :anchored? true)))

(defn remove-anchors [board]
  (into []
        (for [row board]
          (into []
                (for [cell row]
                  (if (floor-cell? cell)
                    (assoc cell :anchored? false)
                    cell))))))

(defn game-over? [board]
  (->> board
       (map (fn [row] (map (fn [cell] (if (and (void-cell? cell) (:piece cell)) true false)) row)))
       (apply concat)
       (some true?)))

(defn move-piece [board [y1 x1 :as from] dest]
  (let [piece (get-in board [y1 x1 :piece])]
    (-> board
        (update-in from place-piece nil)
        (update-in dest place-piece piece))))

(defn push-piece [board [y1 x1 :as from] dest]
  (let [dir-vec (mapv - dest from)
        source-piece (get-in board [y1 x1 :piece])]
    (loop [board (update-in board from place-piece nil)
           source-piece source-piece
           [y2 x2 :as dest] dest]
      (let [dest-piece (get-in board [y2 x2 :piece])
            new-board (update-in board dest place-piece source-piece)]
        (if (nil? dest-piece)
          new-board
          (recur new-board
                 dest-piece
                 (mapv + dir-vec dest)))))))

(defn can-push? [board from dir-vec]
  (let [next-pos (partial mapv + dir-vec)]
    (loop [pos (next-pos from)]
      (let [cell (get-in board pos)]
        (cond
          (void-cell? cell) true
          (open-cell? cell) true
          (anchored? cell) false
          :else (recur (next-pos pos)))))))

(defn valid-push? [board from dest]
  (let [pushing-piece (:piece (get-in board from))
        dest-piece (:piece (get-in board dest))
        dir-vec (mapv - dest from)]
    (cond
      (not (pusher? pushing-piece))                          false ;; "Not a pusher"
      (nil? dest-piece)                                      false ;; "Nothing in the destination to push"
      (not (contains? #{[1 0] [-1 0] [0 1] [0 -1]} dir-vec)) false ;; "Not in the allowed push boundries"
      (not (can-push? board from dir-vec))                   false ;; "Something is blocking this push"
      :else                                                  true)))

(defn get-available-move-pos
  ([board [y x]]
   (let [visited (atom #{})
         up [(- y 1) x]
         down [(+ y 1) x]
         left [y (- x 1)]
         right [y (+ x 1)]]
     (get-available-move-pos board up visited)
     (get-available-move-pos board down visited)
     (get-available-move-pos board left visited)
     (get-available-move-pos board right visited)
     @visited))

  ([board [y x] visited]
   (when (not (contains? @visited [y x]))
     (let [cell (get-in board [y x])
           up [(- y 1) x]
           down [(+ y 1) x]
           left [y (- x 1)]
           right [y (+ x 1)]]
       (when (open-cell? cell)
         (swap! visited set/union #{[y x]})
         (get-available-move-pos board up visited)
         (get-available-move-pos board down visited)
         (get-available-move-pos board left visited)
         (get-available-move-pos board right visited)
         @visited)))))

(defn get-available-push-pos
  ([board [y x]]
   (let [up [(- y 1) x]
         down [(+ y 1) x]
         left [y (- x 1)]
         right [y (+ x 1)]]
     (set (filter (partial valid-push? board [y x]) [up down left right])))))

(defn cell->emoji [cell]
  (let [piece (:piece cell)]
    (cond
      (wall-cell? cell)           "🟫"
      (void-cell? cell)           "⬛"
      (open-cell? cell)           "⬜"
      (anchored? cell)            "🟥"
      (= piece black-square)      "🟪"
      (= piece black-round)       "🟣"
      (= piece white-square)      "🟩"
      (= piece white-round)       "🟢"
      :else "🦥")))

(defn board->emoji [board]
  (map (fn [row] (map cell->emoji row)) board))

(defn pprint-board [board]
  (println (str "  " (string/join "  " (range 10))))
  (let [row-num (atom -1)]
    (println (for [row (board->emoji board)]
               (prn-str (str (string/join " " row) " " (swap! row-num inc))))))
  (println))

(def empty-board (make-standard-board))
; 3 squares 
; 2 rounds 

(def sample-board
  (-> (make-standard-board)
      (update-in [1 4] place-piece white-round)
      (update-in [2 4] place-piece white-square)
      (update-in [3 4] place-piece white-square)
      (update-in [4 4] place-piece white-round)
      (update-in [3 3] place-piece white-square)

      (update-in [1 5] place-piece black-round)
      (update-in [2 5] place-piece black-square)
      (update-in [3 5] place-piece black-square)
      (update-in [4 5] place-piece black-round)
      (update-in [2 6] place-piece black-square)))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn -main [& args]
  (pprint-board (transpose sample-board)))

