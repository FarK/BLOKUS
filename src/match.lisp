(load "ai")

(defun match (ptypes &key (turn 0) max-depth max-time)
  (let ((game (make-game ptypes))
        tile (npasses 0))
    (loop until (match-end-p game npasses) do
          (match-print game turn)
          (if (equal (player-type (game-player game turn)) 'human)
            (setf tile (ask-4-tile game turn))
            (setf tile (minimax game
                                :turn turn
                                :max-time max-time
                                :max-depth max-depth)))
          (case tile
            (pass     (incf npasses))
            (end-game (return-from match nil))
            (t        (game-add-tile game turn tile)))
          (setf turn (game-next-turn game turn)))

    (format t "~%~aPlayer ~a WINS!!~a~%Press some key to continue..."
            (txt-mod "blink")
            (player-id (game-winner game))
            (txt-mod "rst"))
    (get-user-action #'match--key2action)))

(defun match-end-p (game npasses)
  (or (= npasses (game-nplayers game))
      (every #'identity
             (map 'vector #'(lambda (x) (every #'null x))
                  (map 'vector #'player-held-tiles (game-players game))))))

(defun ask-4-tile (game turn)
  (let ((org-board (game-board game))
        (board  (board-copy (game-board game)))
        (player (game-player game turn))
        tile tiles
        (tnum 0) ntiles
        (trot 0)
        (x 0) (y 0))
    (setf (game-board game) board) ; Work with board copy
    (setf tiles (player-collect-htiles player))
    (setf ntiles (array-dimension tiles 0))
    (setf tile (aref tiles 0))

    (loop do
          (board-add-tile board tile (if (game-add-tile-p game tile player)
                                       (player-id player)
                                       -1))
          (match-print game turn)
          (board-rm-tile board tile)
          (setf board (board-copy org-board)) ; Reset board
          (setf (game-board game) board)

          (case (get-user-action #'match--key2action)
            (up        (decf x))
            (down      (incf x))
            (right     (incf y))
            (left      (decf y))
            (next-tile (setf tnum (mod (incf tnum) ntiles)) (setf trot 0))
            (prev-tile (setf tnum (mod (decf tnum) ntiles)) (setf trot 0))
            (next-rot  (setf trot (mod (incf trot) (tile-nrots tile))))
            (prev-rot  (setf trot (mod (decf trot) (tile-nrots tile))))
            (add-tile  (when (game-add-tile-p game tile player) (return)))
            (pass      (return-from ask-4-tile nil))
            (exit      (return-from ask-4-tile 'end-game)))

          (setf tile (make-tile
                       :idx (tile-idx (aref tiles tnum))
                       :rot trot
                       :pos (list x y)))
          (print (board-correct-tile-pos tile))
          (setf x (first  (tile-pos tile)))
          (setf y (second (tile-pos tile)))
      )
    (setf (game-board game) org-board) ; Restore original board
    tile))

(defun match--key2action (key)
  (case key
    (:up      'up)
    (:down    'down)
    (:right   'right)
    (:left    'left)
    (#\t      'next-tile)
    (#\T      'prev-tile)
    (#\r      'next-rot)
    (#\R      'prev-rot)
    (#\Return 'add-tile)
    (#\p      'pass)
    (#\Escape 'exit)))

(defun match-print (game turn)
  (clear-screen)
  (format t "~a~%~a~%" (game-print-points game turn) game))

#|
 | (defvar g)
 | (setf g (make-game '(human paranoid selfish random)))
 | (match '(human paranoid selfish random) :max-depth 2 :max-time 5)
 |#
