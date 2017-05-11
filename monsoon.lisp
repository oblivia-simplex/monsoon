;; a binary rainfall visualizer for data (esp. packets)
;; 0 = red, 1 = green, 2 = blue
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
                         (every #'asciip
                                (loop for j below *ascii-len*
                                      collect
                                      (aref buffer
                                            (min (1- caplen) (+ j i))))))))
        (if (and ascii (not (zerop pixel)))
            (setf (aref row (+ +red+ (* i 3))) (logior #x80 pixel))
            (setf (aref row (+ +green+ (* i 3))) pixel))))
    row))

(defun red-row (width)
  (let ((row (make-array (* width 3) :element-type '(unsigned-byte 8)
                                     :initial-element 0)))
    (loop for i below width do
      (setf (aref row (* i 3)) #xFF))
    row))

(defun make-ppm-header (height width)
  (format nil "P6~%~D ~D~%255~%" width height))

(defun write-row (path buffer caplen counter height width header-len)
  (let ((offset (+ header-len
                   (* (mod counter height) width 3)))
        (row (make-row buffer caplen width)))
    (with-open-file (stream path :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists :overwrite)
      (file-position stream offset)
      (write-sequence row stream)
      (write-sequence (red-row width) stream))
    row))

(defun prepare-canvas (path height width)
  (with-open-file (stream path :if-exists :supersede
                               :element-type 'character ;'(unsigned-byte 8)
                               :direction :output)
    (let ((header (make-ppm-header height width)))
      (write-sequence header stream)
      (length header))))

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
