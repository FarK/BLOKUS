(defstruct (node
             (:constructor make-node
               (state
                &key (turn 0)  max-time-s max-depth
                (max-time (when max-time-s
                            (* max-time-s internal-time-units-per-second)))
                &aux (max-id (if turn turn 0)))))
  ; Modificables
  state
  turn
  max-id
  (depth 0)
  (npasses 0)
  ; Constants
  max-depth
  (start-time (get-internal-real-time))
  max-time
  )

(defun minimax (state &key turn max-depth max-time)
  (let ((node (make-node state
                         :turn (if turn turn 0)
                         :max-time-s max-time
                         :max-depth max-depth))
        values
        best-values
        best-movement)
    (loop for movement in (mm-gen-movements node) until (mm-end-cond node) do
          (mm--next-node node movement)
          (setf values (mm--eval-node node))
          (mm--prev-node node movement)
          (when (mm-choose-values values best-values node)
            (setf best-values values)
            (setf best-movement movement)))
    best-movement))

(defun mm--eval-node (node)
  (let (values new-values)
    (loop for movement in (mm-gen-movements node) until (mm-end-cond node) do
          (mm--next-node node movement)
          (setf new-values (mm--eval-node node))
          (mm--prev-node node movement)
          (when (mm-choose-values new-values values node)
              (setf values new-values)))
  (mm-utility node)))

(defun mm--next-node (node movement)
  (if (eq movement 'pass)
    (incf (node-npasses node))
    (mm-do-movement node movement))
  (setf (node-turn node) (mm-next-turn node))
  (incf (node-depth node)))

(defun mm--prev-node (node movement)
  (decf (node-depth node))
  (setf (node-turn node) (mm-prev-turn node))
  (if (eq movement 'pass)
    (decf (node-npasses node))
    (mm-undo-movement node movement)))
