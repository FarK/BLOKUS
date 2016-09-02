(load "game")
(load "minimax")

; state = game
; movement = tile

(defun mm-end-cond (node)
  (or (mm--max-depth-p  node)
      (mm--max-time-p   node)
      (mm--max-passes-p node)))

(defun mm--max-depth-p (node)
  (let ((depth     (node-depth     node))
        (max-depth (node-max-depth node)))
    (when max-depth (>= depth max-depth))))

(defun mm--max-time-p (node)
  (let ((start (node-start-time node))
        (max   (node-max-time   node)))
  (when max (>= (- (get-internal-real-time) start) max))))

(defun mm--max-passes-p (node)
  (>= (node-npasses node) (game-nplayers (node-state node))))

(defun mm-maxp   (node) (declare (ignore node)) +max-points+)
(defun mm-minp   (node) (declare (ignore node)) 0)
(defun mm-maxsum (node)                         (* +max-points+ (game-nplayers (node-state node))))
(defun mm-minsum (node) (declare (ignore node)) 0)

(defun node-player (node)
  (game-player (node-state node) (node-max-id node)))

(defun mm-gen-movements (node)
  (let ((game      (node-state node))
        (player    (game-player (node-state node) (node-turn node))))
    (append
      (loop for mov in (mm--gen-mov-candidates game player)
            when (game-add-tile-p game mov player) collect mov)
      '(pass))))

(defun mm--gen-mov-candidates (game player)
  (loop for pos in (append (game-valid-pos game player)
                           (list (board-corner (player-id player)))) append
    (loop for h-tile across (player-held-tiles player) when h-tile append
      (loop for rotation in (tile-rotations h-tile) append
        (loop for i-corner in (tile-i-corners h-tile) collect
            (tile-gen-movement (tile-idx h-tile) rotation pos i-corner))))))

(defun mm-do-movement (node movement)
  (game-add-tile (node-state node) (node-turn node) movement))

(defun mm-undo-movement (node movement)
  (game-rm-tile (node-state node) (node-turn node) movement))

(defun mm-next-turn (node)
  (game-next-turn (node-state node) (node-turn node)))

(defun mm-prev-turn (node)
  (mod (decf (node-turn node)) (game-nplayers (node-state node))))

(defun mm-utility (node)
  (list2array (loop for player across (game-players (node-state node))
        collect (player-points player))))

; Returns t when v1 is chosen and nil when v2 is chosen
(defun mm-choose-values (v1 v2 node)
  (unless v2 (return-from mm-choose-values v1))
  (let ((ai-id       (player-id (node-player node)))
        (player-id   (node-turn node))
        (ai-type (player-type (node-player node))))
  (case ai-type
    (random   (zerop (random 2)))
    (selfish  (mm--best-2-me       v1 v2 player-id))
    (paranoid (mm--paranoid-choice v1 v2 ai-id player-id))
    (t        (error "Invalid player type ~a" ai-type)))))

(defun mm--paranoid-choice (v1 v2 ai-id player-id)
  (if (= ai-id player-id)
    (mm--best-2-me   v1 v2 ai-id)
    (mm--worst-2-me  v1 v2 ai-id)))

(defun mm--best-2-me (v1 v2 player-id)
    (mm--vector-i-p v1 v2 player-id #'>))

(defun mm--worst-2-me (v1 v2 player-id)
    (mm--vector-i-p v1 v2 player-id #'<))

; TODO: Desempate
(defun mm--vector-i-p (v1 v2 i test)
  (funcall test (aref v1 i) (aref v2 i)))
