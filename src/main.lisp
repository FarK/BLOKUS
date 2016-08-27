(load "utils")
(load "terminal")
(load "tui")
(load "match")

(defvar +blokus-title+)
(setf +blokus-title+
" ____    ___           __                        
/\\  _`\\ /\\_ \\         /\\ \\                       
\\ \\ \\L\\ \\//\\ \\     ___\\ \\ \\/'\\   __  __    ____  
 \\ \\  _ <'\\ \\ \\   / __`\\ \\ , <  /\\ \\/\\ \\  /',__\\ 
  \\ \\ \\L\\ \\\\_\\ \\_/\\ \\L\\ \\ \\ \\\\`\\\\ \\ \\_\\ \\/\\__, `\\
   \\ \\____//\\____\\ \\____/\\ \\_\\ \\_\\ \\____/\\/\\____/
    \\/___/ \\/____/\\/___/  \\/_/\\/_/\\/___/  \\/___/ ")


(defvar +submenus+)
(setf +submenus+
      '#( ("Player 1"  #(NONE HUMAN SELFISH PARANOID RANDOM)   0)
          ("Player 2"  #(NONE HUMAN SELFISH PARANOID RANDOM)   1)
          ("Player 3"  #(NONE HUMAN SELFISH PARANOID RANDOM)   2)
          ("Player 4"  #(NONE HUMAN SELFISH PARANOID RANDOM)   3)
          ("Max time " #(1 5 10 20 40 80 160 INF)            nil)
          ("Max depth" #(1 2 3 4 5 INF)                      nil) ))

(defun main--key2action (key)
  (case key
    (:up      'up)
    (:down    'down)
    (:right   'right)
    (:left    'left)
    (#\Return 'play)
    (#\Escape 'exit)))

(defun create-submenus ()
  (list2array
    (loop for (title opts color) across +submenus+ collect
          (when opts
            (make-submenu opts
                          :title title
                          :color (if color (box-color color) "white"))))))

(defun init-menu ()
  (let ((menu (make-menu (create-submenus) :title +blokus-title+)))
    (loop do
          (clear-screen)
          (format t "~%~a~%~%~aPress ENTER to start or ESC to exit~a~%"
                  menu (txt-mod "blink") (txt-mod "rst"))
          (case (get-user-action #'main--key2action)
            (up        (menu-prev-sm  menu))
            (down      (menu-next-sm  menu))
            (right     (menu-next-opt menu))
            (left      (menu-prev-opt menu))
            (play      (return nil))
            (exit      (exit))))

    ; Returns selected options
    (loop for submenu across (menu-submenus menu) collect
          (aref (submenu-opts submenu) (submenu-idx submenu)))))

(defun options-players (options)
  (loop for player in (subseq options 0 4)
        unless (eq player 'NONE) collect player))

(defun options-max-time (options)
  (unless (eq (nth 4 options) 'INF) (nth 4 options)))

(defun options-max-depth (options)
  (unless (eq (nth 5 options) 'INF) (nth 5 options)))

(defun main ()
  (loop do
    (let ((options (init-menu)))
      (when (options-players options)
        (match (options-players options)
               :max-time (options-max-time options)
               :max-depth (options-max-depth options))))))
