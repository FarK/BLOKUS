(defun get-user-action (key2action)
  (let (in key action)
    (loop do
      (setf in (ext:with-keyboard t (read-char ext:*keyboard-input*)))
      (setf key (if (system::input-character-key in)
                  (system::input-character-key  in)
                  (system::input-character-char in)))
      (when (setf action (funcall key2action key)) (return action)))))

(defstruct (menu
             (:print-function (lambda (menu stream depth)
                                (declare (ignore stream) (ignore depth))
                                (print-menu menu)))
             (:constructor make-menu
                           (submenus
                            &key title
                            &aux (nmenus (array-dimension submenus 0)))))
  (idx 0)
  (nmenus 0)
  title
  submenus
  )

(defstruct (submenu
             (:print-function (lambda (submenu stream depth)
                                (declare (ignore stream) (ignore depth))
                                (format t "~a" (submenu2str submenu))))
             (:constructor make-submenu
                           (opts
                            &key title color
                            &aux (nopts (array-dimension opts 0)))))

  (idx 0)
  (nopts 0)
  title
  (color "white")
  opts
  )

(defun menu-submenu (menu)
  (aref (menu-submenus menu) (menu-idx menu)))

(defun menu-next-sm (menu)
  (setf (menu-idx menu) (mod (1+ (menu-idx menu)) (menu-nmenus menu))))

(defun menu-prev-sm (menu)
  (setf (menu-idx menu) (mod (1- (menu-idx menu)) (menu-nmenus menu))))

(defun menu-next-opt (menu)
  (let ((submenu (menu-submenu menu)))
    (setf (submenu-idx submenu) (mod (1+ (submenu-idx submenu))
                                     (submenu-nopts submenu)))))
(defun menu-prev-opt (menu)
  (let ((submenu (menu-submenu menu)))
    (setf (submenu-idx submenu) (mod (1- (submenu-idx submenu))
                                     (submenu-nopts submenu)))))

(defun print-menu (menu)
  (let ((idx      (menu-idx      menu))
        (nmenus   (menu-nmenus   menu))
        (title    (menu-title    menu))
        (submenus (menu-submenus menu)))
    (format t "~a~%~%~{~a~%~}"
            (if title title "")
            (loop for i from 0 below nmenus collect
                  (format nil "~a~a"
                          (if (= i idx) "--> " "    ")
                          (submenu2str (aref submenus i)))))))


(defun submenu2str (submenu)
  (let ((idx     (submenu-idx     submenu))
        (nopts   (submenu-nopts   submenu))
        (title   (submenu-title   submenu))
        (color   (submenu-color   submenu))
        (opts    (submenu-opts    submenu)))
    (format nil "~a~a~a~{~a~^    ~}"
            (txt-mod (string-downcase color))
            (if title (concatenate 'string title ":  ") "")
            (txt-mod "rst")
            (loop for i from 0 below nopts collect
                  (format nil "~a~a~a"
                          (if (= i idx) (txt-mod "BLUE") "")
                          (aref opts i)
                          (txt-mod "rst"))))))
