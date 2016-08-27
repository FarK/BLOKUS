(load "board")

(defvar +player-types+ '(human random selfish paranoid))

(defstruct (player
             (:constructor make-player
               (id ptype
                &aux (type
                       (if (find ptype +player-types+)
                         ptype
                         (error "Invalid player type ~a. Valid types are ~a"
                                ptype +player-types+))))))
  (id          0)
  (type        'human) ; 'human, 'selfish, 'paranoid
  (points 0)
  (held-tiles  (starting-tiles))
  (added-tiles (make-array (list +ntiles+)))
  (nadded 0)
  (corner (board-corner id))
  )

(defun player-add-tile (player tile)
  (let ((atiles (player-added-tiles player))
        (htiles (player-held-tiles  player))
        (tile-idx (tile-idx tile))
        (pos      (tile-pos tile))
        (rot      (tile-rot tile)))
    (progn
      (setf (aref atiles tile-idx) (aref htiles tile-idx))
      (setf (tile-pos (aref atiles tile-idx)) pos)
      (setf (tile-rot (aref atiles tile-idx)) rot)
      (setf (aref htiles tile-idx) nil)
      (incf (player-points player) (tile-points tile-idx))
      (incf (player-nadded player)))))

(defun player-rm-tile (player tile)
  (let ((tile-idx (tile-idx tile))
        (atiles (player-added-tiles player))
        (htiles (player-held-tiles  player)))
    (progn
      (setf (aref htiles tile-idx) (aref atiles tile-idx))
      (setf (tile-pos (aref htiles tile-idx)) '(0 0))
      (setf (tile-rot (aref htiles tile-idx)) 0)
      (setf (aref atiles tile-idx) nil)
      (decf (player-points player) (tile-points tile-idx))
      (decf (player-nadded player)))))

; Not nil htiles
(defun player-collect-htiles (player)
  (list2array
    (loop for htile across (player-held-tiles player) when htile collect htile)))
