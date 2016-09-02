(load "player")

(defstruct (game
             (:print-function (lambda (game stream depth)
                                (declare (ignore stream) (ignore depth))
                                (board-print (game-board game))))
             (:constructor make-game
               (ptypes
                 &aux (players
                        (let ((id -1))
                          (when (> (list-length ptypes) 4)
                            (error "Four maximum players"))
                          (list2array (loop for type in ptypes collect
                                            (make-player (incf id) type))))))))
  (board (make-board))
  players
  (nplayers (array-dimension players 0))
  )

(defun game-player (game player-id)
  (aref (game-players game) player-id))

(defun game-shallow-copy (game)
  (let ((new-game (make-game '())))
    (setf (game-board new-game)    (board-copy (game-board game)))
    (setf (game-players new-game)  (game-players game))
    (setf (game-nplayers new-game) (game-nplayers game))
    new-game))

(defun game-add-tile-p (game tile player)
  (if (zerop (player-nadded player))
    (board-add-first-tile-p (game-board game) tile (player-id player))
    (board-add-tile-p (game-board game) tile (player-id player))))

(defun game-add-tile (game player-id tile)
  (player-add-tile (game-player game player-id) tile)
  (board-add-tile  (game-board game) tile player-id))

(defun game-rm-tile (game player-id tile)
  (player-rm-tile (game-player game player-id) tile)
  (board-rm-tile (game-board game) tile))

(defun game-scores (game)
  (loop for player across (game-players game) collect (palyer-points player)))

(defun game-next-turn (game current-turn)
  (mod (1+ current-turn) (game-nplayers game)))

(defun game-valid-pos (game player)
  (remove-duplicates
    (loop for a-tile across (player-added-tiles player) when a-tile append
          (game--valid-corners (game-board game) a-tile (player-id player)))
    :test #'equal))

(defun game--valid-corners (board a-tile player-id)
  (let (glob-c)
    (loop for e-corner in (tile-e-corners a-tile)
          when (progn
                 (setf glob-c (add-coords (tile-pos a-tile) e-corner))
                 (and (board-check-box board glob-c)
                      (board-check-box-edges board glob-c player-id)))
          collect glob-c)))

(defun game-winner (game)
  (let ((max-points 0) winner)
    (loop for player across (game-players game) do
          (when (>= (player-points player) max-points)
            (setf max-points (player-points player))
            (setf winner player)))
    winner))

(defun game-print-points (game turn)
  (format nil "~{~a~^~%~%~}"
    (loop for p across (game-players game) collect
          (format nil "~aPlayer ~a~a: ~2d ~a~{~a~}~a"
                  (if (= (player-id p) turn) (txt-mod "blink") "")
                  (player-id p)
                  (if (= (player-id p) turn) (txt-mod "rst")   "")
                  (player-points p)
                  (txt-mod (box-color (player-id p)))
                  (loop for sp from 0 below (player-points p) collect " ")
                  (txt-mod "rst")))))
