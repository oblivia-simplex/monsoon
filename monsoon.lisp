;; a binary rainfall visualizer for data (esp. packets)
;; 0 = red, 1 = green, 2 = blue

(in-package :monsoon)

(defparameter *debug* nil)
(defparameter *colour* 1)
(defvar +red+ 0)
(defvar +green+ 1)
(defvar +blue+ 2)

(defparameter *highlight-ascii* t)

(defun asciip (b)
  (< #x1F b #x80))

(defparameter *ascii-len* 4)

(defun make-row (buffer caplen width)
  (let ((row (make-array (* width 3) :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
    (loop for i below width do
      (let* ((pixel (if (>= i caplen)
                        0
                        (aref buffer i)))
             (ascii (and *highlight-ascii*
                         (or  (every #'asciip
                                     (loop for j below *ascii-len*
                                           collect
                                           (aref buffer
                                                 (min (1- caplen)
                                                      (+ j i)))))
                              (every #'asciip
                                     (loop for j below *ascii-len*
                                           collect
                                           (aref buffer
                                                 (max 0
                                                      (- i j)))))))))

        (if (and ascii (not (zerop pixel)))
            (setf (aref row (+ +red+ (* i 3))) (logior #x80 pixel))
            (setf (aref row (+ +green+ (* i 3))) pixel))))
    row))

(defun scan-row (width)
  (let ((row (make-array (* width 3) :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
    (loop for i below width do
      (setf (aref row (+ +blue+ (* i 3))) #xFF))
    row))

(defun write-row (path buffer caplen counter height width header-len)
  (let ((offset (+ header-len
                   (* (mod counter height) width 3)))
        (row (make-row buffer caplen width)))
    (with-open-file (stream path :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :overwrite)
      (file-position stream offset)
      (write-sequence row stream)
      (write-sequence (scan-row width) stream))
    row))

(defun color-offset (arr i color-idx)
  (aref arr (+ color-idx (* i 3))))

(defun sdl-color-from-row (row i)
  (sdl:color :r (color-offset row i +red+)  
             :g (color-offset row i +green+)
             :b (color-offset row i +blue+)))

(defun show-row (buffer caplen counter height width)
  (let ((y (mod counter height))
        (blue (sdl:color :r 0 :b #xFF :g 0)) 
        (row (make-row buffer caplen width)))
    (loop for x below width do
      (let ((color (sdl-color-from-row row x)))
        (sdl:draw-pixel-* x y :color (sdl-color-from-row row x))
        (sdl:draw-pixel-* x (mod (1+ y) height) :color blue)
        (sdl:free color)))
    (sdl:free blue)))

(defun sniff (pcap-path image-path interface
              &key (snaplen 512)
                (rotate-at 512)
                (count 512)
                (promisc t)
                (packets nil)
                (filter nil))
  (let ((counter 0)
        (imagebuffer (make-array (* snaplen rotate-at 3)
                                 :element-type '(unsigned-byte 8)))
        (header-len (prepare-canvas image-path rotate-at snaplen)))
    (with-pcap-interface (pcap interface :promisc promisc
                                         :snaplen snaplen
                                         :nbio t)
      (with-pcap-writer (writer pcap-path :snaplen snaplen
                                          :datalink (pcap-live-datalink pcap))
        (when filter
          (set-filter pcap filter))
        (loop
          (when (and packets (> counter packets))
            return)
          (capture pcap 64
                   (lambda (sec usec caplen len buffer)
                     (dump writer buffer sec usec :length caplen :origlength len)
                     (write-row image-path
                                buffer
                                caplen
                                counter
                                rotate-at
                                snaplen
                                header-len)
                     (incf counter)
                     (format t "[~D] Packet length: ~A bytes (~A), on the wire: ~A bytes~%" counter caplen (length buffer) len)))
          (sleep 0.001))))))

(defparameter *rate* 2)

(defun vid-sniff (&key
                    (interface "wlp3s0")
                    (image-path nil)
                    (pcap-path "/dev/null")
                    (snaplen (video-width :init t))
                    (rotate-at  (video-height :init t))
                    (promisc t)
                    (filter nil))
  (let ((counter 0)
        (header-len (when image-path
                      (prepare-canvas image-path rotate-at snaplen))))
    (with-graphics (:width snaplen
                    :height rotate-at
                    :frame-rate -1 
                    :flags (list sdl:sdl-resizable))

      (with-pcap-interface (pcap interface :promisc promisc
                                           :snaplen (video-width); snaplen
                                           :nbio t)
        (with-pcap-writer (writer pcap-path :snaplen (video-width);snaplen
                                            :datalink (pcap-live-datalink pcap))
          (when filter
            (set-filter pcap filter))
          (frame-loop ()
              (capture pcap *rate*
                       (lambda (sec usec caplen len buffer)
                         (dump writer buffer sec usec
                               :length caplen
                               :origlength len)
                         (show-row buffer
                                   caplen
                                   counter
                                   (video-height)
                                   (video-width))
                         (when image-path
                           (write-row image-path
                                      buffer
                                      caplen
                                      counter
                                      rotate-at
                                      snaplen
                                      header-len))
                         (incf counter)
                         (when *debug*
                           (format t "[~D] Packet length: ~A bytes (~A), on the wire: ~A bytes~%" counter caplen (length buffer) len))))
            (sleep 0.01)))))))
  
