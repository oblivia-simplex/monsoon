(in-package :monsoon)

(defun earmuff->string (symbol)
  (let ((string (remove #\* (symbol-name symbol))))
    string))

(defun tweakable->posixopt (symbol)
  (concatenate 'string "--"
               (string-downcase
                (earmuff->string symbol))))

(defun get-opt-arg (list key)
  (let ((anything-there (member key list :test #'equalp)))
    (when (cdr anything-there)
      (cadr anything-there))))

(defun print-help ()
  (format t "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%")
  (format t "                                 MONSOON~%")
  (format t "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=~%~%")
  (print-tweakables)
  :EXIT)

(defun hrule ()
  (loop for i below 30 do
    (princ "=-"))
  (terpri))

(defun print-tweakables ()
  (loop
    for symbol in *tweakables*
    for i from 0 to (length *tweakables*) do
      (format t "[~d] ~A  ~S~%     ~A~%" i (tweakable->posixopt symbol)
              (symbol-value symbol)
              (documentation symbol 'variable))))

(defun parse-command-line-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (cond ((or (member "--help" args :test #'equalp)
               (member "-h" args :test #'equalp))
           (print-help))
          (t (hrule)
             (loop for param in *tweakables* do
               (let ((key (tweakable->posixopt param)))
                    (when (member key args :test #'equalp)
                      ;; (FORMAT T "FOUND OPT: ~S = ~S~%"
                      ;; key (get-opt-arg args key))
                      (let ((opt (get-opt-arg args key)))
                        (setf (symbol-value param)
                              (if (get param 'string)
                                  opt
                                  (read-from-string (get-opt-arg args key)))))
                      (format t "[+] SETTING ~A TO ~A...~%"
                              param (symbol-value param))))) T))))

(defun main ()
  (unless (eq (print (parse-command-line-args)) :EXIT)
    (vid-sniff)))
