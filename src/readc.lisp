(defstruct key char bits font key)
(defun readc ()
  (let ((c #\a))
    (loop while (not (eq (ignore-errors (character c)) #\q)) do
        (ext:with-keyboard t
          (format t "press q to exit ")
          (setf c (read-char ext:*keyboard-input*))
          (format t "~d~%"  (system::input-character-key c))))))
;#S(INPUT-CHARACTER :CHAR NIL :BITS 8 :FONT 0 :KEY UP)
(with-open-stream (w (screen:make-window))
  (readc))
