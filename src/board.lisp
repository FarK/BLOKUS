(load "tiles")

(defvar +board-width+)
(defvar +board-height+)
(setf +board-width+  20)
(setf +board-height+ 20)

(defun board-corner (id)
  (case id
    (0 (list 0                                   0))
    (1 (list (1- +board-height+) (1- +board-width+)))
    (2 (list 0                   (1- +board-width+)))
    (3 (list (1- +board-height+)                  0))
    (t (error "Square have only four corners"))))

(defun make-board ()
  (make-array '(20 20)))

(defun board-copy (board)
  (let ((new-board (make-array (array-dimensions board))))
    (loop for x from 0 below +board-height+ do
      (loop for y from 0 below +board-width+ do
            (setf (aref new-board x y) (aref board x y))))
  new-board))

(defun board-add-tile (board tile id)
  (mapcar #'(lambda (pos) (board-set-box board id pos))
          (tile-coords tile)))

(defun board-rm-tile (board tile)
  (mapcar #'(lambda (pos) (board-set-box board nil pos))
          (tile-coords tile)))

(defun board-set-box (board id pos)
  (setf (apply #'aref board pos) id))

(defun board-add-tile-p (board tile id)
  (let ((tile-coords (tile-coords tile)))
    (and
      (every #'(lambda (x) (and (board-check-box         board x)
                                (board-check-box-edges   board x id)))
             tile-coords)
      (some  #'(lambda (x)      (board-check-box-corners board x id))
             tile-coords))))

(defun board-add-first-tile-p (board tile id)
  (let ((tile-coords (tile-coords tile)))
    (and
      (some  #'(lambda (x) (equal x (board-corner id))) tile-coords)
      (every #'(lambda (x) (board-check-box board x))   tile-coords))))

(defun board-check-box (board coord)
  (let ((x (first coord)) (y (second coord)))
    (and (array-in-bounds-p board x y)
         (null (aref board x y)))))

(defun board-check-box-edges (board coord id)
  (loop for (x y) in (pos-edges coord) never
        (and (array-in-bounds-p board x y)
             (eq (aref board x y) id))))

(defun board-check-box-corners (board coord id)
  (loop for (x y) in (pos-corners coord) thereis
        (and (array-in-bounds-p board x y)
             (eq (aref board x y) id))))

(defun board-next-pos (pos)
  (let ((x (first pos)) (y (second pos)))
    (if (< (1+ y) +board-width+)
      (list x (1+ y))
      (if (< (1+ x) +board-height+)
        (list (1+ x) 0)
        nil))))

(defvar +box-color+)
(setf +box-color+ (make-hash-table :test 'equal))
(setf (gethash -1 +box-color+) "WHITE"  )
(setf (gethash 0 +box-color+) "BLUE"   )
(setf (gethash 1 +box-color+) "RED"    )
(setf (gethash 2 +box-color+) "YELLOW" )
(setf (gethash 3 +box-color+) "CYAN"   )
(setf (gethash 4 +box-color+) "MAGENTA")
(setf (gethash 6 +box-color+) "GREEN"  )
(setf (gethash 7 +box-color+) "BLACK"  )

(defun box-color (id)
  (gethash id +box-color+))

(defvar +gp-row+)
(setf +gp-row+
    (format nil "~{~a~}+"
            (loop for i from 0 below (* 4 20) collect
                  (if (zerop (mod i 4)) "+" "-"))))

(defun board-print (board)
  (format t (concatenate 'string "~{" +gp-row+ "~%~{|~a~}|~%~}" +gp-row+)
          (loop for i below +board-height+ collect
            (loop for j below +board-width+ collect (block2str board i j)))))

(defun block2str (board x y)
  (let ((box (aref board x y)))
    (format nil "~a   ~a"
            (txt-mod (if box (box-color box) "rst"))
            (txt-mod "rst"))))

(defun board-correct-tile-pos (tile)
  (let ((x (first (tile-pos tile)))
        (y (second (tile-pos tile)))
        (theight (first  (tile-dimensions tile)))
        (twidth  (second (tile-dimensions tile)))
        maxx maxy)
    (setf maxx (+ x theight -1))
    (setf maxy (+ y twidth -1))
    (when (>= maxx +board-height+) (setf x 0))
    (when (>= maxy +board-width+)  (setf y 0))
    (when (<  x 0)                 (setf x (- +board-height+ theight)))
    (when (<  y 0)                 (setf y (- +board-height+ twidth)))
    (setf (first  (tile-pos tile)) x)
    (setf (second (tile-pos tile)) y))
  tile)
