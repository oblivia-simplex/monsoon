(in-package :monsoon)


(defun make-ppm-header (height width)
  (format nil "P6~%~D ~D~%255~%" width height))

(defun prepare-canvas (path height width)
  (with-open-file (stream path :if-exists :supersede
                               :element-type 'character ;'(unsigned-byte 8)
                               :direction :output)
    (let ((header (make-ppm-header height width)))
      (write-sequence header stream)
      (length header))))

(defun video-width (&key (init nil))
  (when init (sdl:init-video))
  (let ((vd (sdl:video-dimensions)))
    (if (null vd)
        512
        (aref (sdl:video-dimensions) 0))))

(defun video-height (&key (init nil))
  (when init (sdl:init-video))
  (let ((vd (sdl:video-dimensions)))
    (if (null vd)
        512
        (aref (sdl:video-dimensions) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thanks to _dead from #lisp for this these SDL macros:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro with-graphics ((&key (width 640) (height 480)
                            (frame-rate 30)
                            (flags ())
                            (bpp 32)) &body forms)
  `(block nil
     (unwind-protect
          (when (sdl:init-sdl)
            (sdl:initialize-subsystems-on-startup)
            (sdl:quit-subsystems-on-exit)
            (sdl:window ,width ,height :bpp ,bpp
                                       :flags ,flags)
            (setf (sdl:frame-rate) ,frame-rate)
            (progn ;let ((*palette* ,palette))
              ,@forms))
       (sdl:quit-sdl))))


(defun call-with-frame-function (frame-function &key one-time)
  (sdl:with-surface (disp sdl:*default-display*)
    (when one-time
      (funcall frame-function)
      (sdl:update-display))
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (cond ((sdl:key= key :sdl-key-escape)
                              (sdl:push-quit-event))
                             ((sdl:key= key :sdl-key-1)
                              (setq *rate* 1))
                             ((sdl:key= key :sdl-key-2)
                              (setq *rate* 2))
                             ((sdl:key= key :sdl-key-3)
                              (setq *rate* 4))
                             ((sdl:key= key :sdl-key-4)
                              (setq *rate* 8))
                             ((sdl:key= key :sdl-key-5)
                              (setq *rate* 16))
                             ((sdl:key= key :sdl-key-6)
                              (setq *rate* 32))
                             ((sdl:key= key :sdl-key-7)
                              (setq *rate* 64))
                             ((sdl:key= key :sdl-key-8)
                              (setq *rate* 128))
                             ((sdl:key= key :sdl-key-9)
                              (setq *rate* 256))
                             ((sdl:key= key :sdl-key-0)
                              (setq *rate* 512))
                             ((sdl:key= key :sdl-key-minus)
                              (setq *rate* -1))
                             ((sdl:key= key :sdl-key-a)
                              (setq *highlight-ascii*
                                    (not *highlight-ascii*)))
                             ((sdl:key= key :sdl-key-b)
                              (setq *mode* :bitwise))
                             ((sdl:key= key :sdl-key-h)
                              (setq *mode* :heatmap))
                             ((sdl:key= key :sdl-key-n)
                              (setq *mode* :bytewise)))
                       )
      (:idle ()
             (when (not one-time)
               (funcall frame-function)
               (sdl:update-display))))))

(defmacro frame-loop ((&key one-time) &body forms)
  `(call-with-frame-function (lambda () ,@forms) :one-time ,one-time))
