(load "utils")
(load "match")

(defun aw--get-battles ()
  '((selfish  selfish  selfish  selfish)
    (paranoid paranoid paranoid paranoid)
    (random   random   random   random)

    (selfish  selfish  selfish  paranoid)
    (selfish  selfish  paranoid paranoid)
    (selfish  paranoid paranoid paranoid)

    (selfish  selfish  selfish  random)
    (selfish  selfish  random   random)
    (selfish  random   random   random)

    (paranoid paranoid paranoid random)
    (paranoid paranoid random   random)
    (paranoid random   random   random)

    (selfish  selfish  paranoid random)
    (selfish  paranoid paranoid random)
    (selfish  paranoid random   random)
    ))

(defun aw--match (ptypes max-depth max-time)
  (let ((game (make-game ptypes))
        tile (npasses 0)
        (turn 0))
    (loop until (match-end-p game npasses) do
          (setf tile (minimax game
                              :turn turn
                              :max-time max-time
                              :max-depth max-depth))
          (case tile
            (pass     (incf npasses))
            (t        (game-add-tile game turn tile)))
          (setf turn (game-next-turn game turn)))

    (format nil "~{~a~^, ~};~a" ptypes (player-id (game-winner game)))))

(defun ai-war ()
  (let ((battles (aw--get-battles))
        (i 0)
        (outf (open "./ai-war.out" :direction :output :if-exists :supersede)))

    (loop for battle in battles do
          (format t "~a/~a~%" (incf i) (length battles))
          (write-line (aw--match battle nil 3) outf))
    (close outf)))
