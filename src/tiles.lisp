(defvar +btiles+) ; base-tiles
(defvar +btiles-points+)
(defvar +tiles+)
(defvar +tiles-coords+)
(defvar +tiles-external-corners+)
(defvar +tiles-internal-corners+)

(defvar +ntiles+)
(defvar +max-points+)
(defvar +min-points+)

(defstruct tile
  (idx 0)      ; +tiles+ index
  (rot 0)      ; rotation index
  (pos '(0 0)) ; translation
  )

(defun tile-coords (tile)
  (mapcar #'(lambda (coord) (add-coords (tile-pos tile) coord))
          (aref (aref +tiles-coords+ (tile-idx tile)) (tile-rot tile))))

(defun tile-rotations (tile)
  (loop for rot from 0 below (array-dimension (aref +tiles+ (tile-idx tile)) 0)
        collect rot))

(defun tile-nrots (tile)
  (array-dimension (aref +tiles+ (tile-idx tile)) 0))

(defun tile-dimensions (tile)
  (let ((btile (tile-get-btile tile)))
    (list (list-length btile) (list-length (car btile)))))

(defun tile-e-corners (tile)
  (aref (aref +tiles-external-corners+ (tile-idx tile)) (tile-rot tile)))

(defun tile-i-corners (tile)
  (aref (aref +tiles-internal-corners+ (tile-idx tile)) (tile-rot tile)))

(defun tile-gen-movement (tile-idx rot pos i-coord)
  (make-tile
    :idx tile-idx
    :rot rot
    :pos (sub-coords pos i-coord)))

(defun starting-tiles ()
  (list2array (loop for i below +ntiles+ collect (make-tile :idx i))))

(defun tile-points (tile-idx)
  (aref +btiles-points+ tile-idx))

(defun tile-get-btile (tile)
  (aref (aref +tiles+ (tile-idx tile)) (tile-rot tile)))

(defun btile-transpose (l) (apply #'mapcar #'list l))
(defun btile-rev-rows (l)  (reverse l))
(defun btile-rev-cols (l)  (mapcar 'reverse l))

(defun btile-rotate (bt rot)
  (case rot
    (0   bt)
    (90  (btile-rev-cols(btile-transpose bt)))
    (180 (btile-rev-cols(btile-rev-rows bt)))
    (270 (btile-rev-rows(btile-transpose bt)))
    (otherwise (error "Invalid rotation"))))

(defun btile-rotations (bt)
  (list2array
    (delete-duplicates
      (loop for rot in '(0 90 180 270) collect (btile-rotate bt rot))
      :test 'equal)))

(defun btile-coords (btile)
  (let ((heigth (list-length btile))
        (width  (list-length (first btile))))
    (let ((matrix (make-array (list heigth width) :initial-contents btile)))
      (loop for i below heigth append
            (loop for j below width
                  when (aref matrix i j) collect (list i j))))))

(defun tile-e-corner-p (t-coords corner)
  (unless (find corner t-coords :test 'equal)
    (notany #'(lambda (edge) (find edge t-coords :test 'equal))
            (pos-edges corner))))

(defun tile-i-corner-p (t-coords corner)
  (some #'(lambda (L) (notany #'(lambda (l) (find l t-coords :test 'equal)) L))
          (pos-Ls corner)))

(defun tile-gen-e-corners (t-coords)
  (remove-duplicates
    (loop for coord in t-coords append
        (loop for corner in (pos-corners coord)
              when (tile-e-corner-p t-coords corner) collect corner))))

(defun tile-gen-i-corners (t-coords)
  (loop for coord in t-coords when (tile-i-corner-p t-coords coord)
          collect coord))

(defun gen-tiles ()
  (map 'array #'btile-rotations +btiles+))

(defun gen-tiles-coords ()
  (map 'array #'(lambda (tile) (map 'array #'btile-coords tile)) +tiles+))

(defun gen-tiles-external-corners ()
  (map 'array #'(lambda (t-coords) (map 'array #'tile-gen-e-corners t-coords))
       +tiles-coords+))

(defun gen-tiles-internal-corners ()
  (map 'array #'(lambda (t-coords) (map 'array #'tile-gen-i-corners t-coords))
       +tiles-coords+))

(defun btile-points (btile)
  (apply #'+ (mapcar #'(lambda (row) (count-if #'identity row)) btile)))

(defun gen-btiles-points ()
  (list2array (mapcar #'btile-points +btiles+)))

; Debug purpose
(defun print-all-tiles ()
  (format t "~{~a~^~%~}"
          (loop for idx below 21 append
                (let ((tile-rotations (aref +tiles+ idx)))
                  (loop for rot below (array-dimension tile-rotations 0) collect
                        (make-tile :idx idx :rot rot))))))

(defun tile-print (tile)
  (let ((bt (tile-get-btile tile)))
    (format t "~{~a~^~%~}~%~%"
            (loop for row in bt collect
                  (format nil "~{~a~}"
                          (mapcar #'(lambda (c) (cond ((null c) " ")
                                                       (t       "#")))
                                  row))))))

(setf +btiles+ (sort
      '((( t ))

        (( t   t ))

        (( t   t )
         (nil  t ))

        (( t   t   t ))

        (( t   t )
         ( t   t ))

        ((nil  t  nil)
         ( t   t   t ))

        (( t   t   t   t ))

        ((nil nil  t )
         ( t   t   t ))

        ((nil  t   t )
         ( t   t  nil))

        (( t  nil nil nil)
         ( t   t   t   t ))

        ((nil  t  nil)
         (nil  t  nil)
         ( t   t   t ))

        (( t  nil nil)
         ( t  nil nil)
         ( t   t   t ))

        ((nil  t   t   t )
         ( t   t  nil nil))

        ((nil nil  t )
         ( t   t   t )
         ( t  nil nil))

        (( t   t   t   t   t ))

        (( t   t   t )
         ( t   t  nil))

        ((nil  t   t )
         ( t   t  nil)
         ( t  nil nil))

        (( t   t   t )
         ( t  nil  t ))

        ((nil  t   t )
         ( t   t  nil)
         (nil  t  nil))

        ((nil  t  nil)
         ( t   t   t )
         (nil  t  nil))

        ((nil  t  nil nil)
         ( t   t   t   t )))
      #'(lambda (a b) (> (btile-points a) (btile-points b)))))

(setf +btiles-points+ (gen-btiles-points))
(setf +tiles+ (gen-tiles))
(setf +tiles-coords+ (gen-tiles-coords))
(setf +tiles-external-corners+ (gen-tiles-external-corners))
(setf +tiles-internal-corners+ (gen-tiles-internal-corners))

(setf +ntiles+ (array-dimension +tiles+ 0))
(setf +max-points+ (apply #'+ (map 'list #'identity +btiles-points+)))
(setf +min-points+ 0)
