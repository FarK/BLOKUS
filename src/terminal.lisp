(defvar +txt-mode+)
(setf +txt-mode+ (make-hash-table :test 'equal))
(setf (gethash "rst"     +txt-mode+)  0)
(setf (gethash "blink"   +txt-mode+)  5)

; Foreground colors
(setf (gethash "black"   +txt-mode+) 30)
(setf (gethash "red"     +txt-mode+) 31)
(setf (gethash "green"   +txt-mode+) 32)
(setf (gethash "yellow"  +txt-mode+) 33)
(setf (gethash "blue"    +txt-mode+) 34)
(setf (gethash "magenta" +txt-mode+) 35)
(setf (gethash "cyan"    +txt-mode+) 36)
(setf (gethash "white"   +txt-mode+) 37)

; Background colors
(setf (gethash "BLACK"   +txt-mode+) 40)
(setf (gethash "RED"     +txt-mode+) 41)
(setf (gethash "GREEN"   +txt-mode+) 42)
(setf (gethash "YELLOW"  +txt-mode+) 43)
(setf (gethash "BLUE"    +txt-mode+) 44)
(setf (gethash "MAGENTA" +txt-mode+) 45)
(setf (gethash "CYAN"    +txt-mode+) 46)
(setf (gethash "WHITE"   +txt-mode+) 47)

(defun txt-mod (modes)
  (format nil "~c[~{~a~^;~}m" #\ESC
          (loop for m in (cond ((listp modes) modes) (t (list modes))) collect
                (gethash m +txt-mode+))))

(defun clear-screen ()
  (screen:with-window (screen:clear-window screen:*window*)))
