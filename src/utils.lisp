(defun list2array (l)
    (make-array (list (list-length l)) :initial-contents l))

(defun add-coords (c1 c2)
  (mapcar #'+ c1 c2))

(defun sub-coords (c1 c2)
  (mapcar #'- c1 c2))

(defun pos-edges (pos)
  (mapcar #'(lambda (a) (add-coords pos a))
          '( (-1 0) (0 1) (1 0) (0 -1))))

(defun pos-corners (pos)
  (mapcar #'(lambda (a) (add-coords pos a))
          '( (-1 -1) (-1 1) (1 1) (1 -1))))

(defvar +Ls+)
(setf +Ls+ '((( 0 -1) (-1 -1) (-1  0))   ; NW
             ((-1  0) (-1  1) ( 0  1))   ; NE
             (( 0  1) ( 1  1) ( 1  0))   ; SE
             (( 1  0) ( 1 -1) ( 0 -1)))) ; SW
(defun pos-Ls (pos)
  (loop for L in +Ls+ collect
    (mapcar #'(lambda (a) (add-coords pos a)) L)))

; Do not keep order
(defun rem-dup-coords (list)
  (loop for i on (sort list #'coords-lt) unless (equal (first i) (second i))
        collect (first i)))

(defun coords-lt (c1 c2)
  (let ((x1 (first c1)) (y1 (second c1))
        (x2 (first c2)) (y2 (second c2)))
  (if (< x1 x2) t (when (= x1 x2) (< y1 y2)))))
