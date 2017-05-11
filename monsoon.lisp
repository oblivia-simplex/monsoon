;; a binary rainfall visualizer for data (esp. packets)
;; 0 = red, 1 = green, 2 = blue
(defparameter *colour* 1)
(defvar +red+ 0)
(defvar +green+ 1)
(defvar +blue+ 2)

;; inefficient. It would be nice to just update the png file by editing
;; the header and then appending the data.
(defun write-image (path buffer imagebuffer counter height width)
  (with-open-file (stream path :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
    (let ((offset (* (mod counter height) width 3))
          (next-offset (* (mod (1+ counter) height) width 3)))
      (loop for i below width do
        (let ((pixel (if (>= i (length buffer))
                         0
                         (aref buffer i))))
          (setf (aref imagebuffer (+ +green+ offset (* i 3)))
                (aref buffer i))
          (setf (aref imagebuffer (+ +red+ offset (* i 3)))
                0)
          (setf (aref imagebuffer (+ +red+ next-offset (* i 3)))
                #xFF)))
      (format t ">> offset: ~S~%" offset)
      (when (zerop (mod counter 32))
        (zpng:write-png-stream
         (make-instance 'zpng:png
                        :color-type :truecolor
                        :height height
                        :width width
                        :bpp 8
                        :image-data imagebuffer)
         stream)))))


(defun sniff (pcap-path image-path interface
              &key (snaplen 512)
                (rotate-at 512)
                (count 512))
  (let ((counter 0)
        (imagebuffer (make-array (* snaplen rotate-at 3)
                                 :element-type '(unsigned-byte 8))))
    (with-pcap-interface (pcap interface :promisc t
                                         :snaplen snaplen
                                         :nbio t)
      (with-pcap-writer (writer pcap-path :snaplen 512
                                          :datalink (pcap-live-datalink pcap))
                                        ;(set-filter pcap "ip")
        (loop
          (capture pcap count
                   (lambda (sec usec caplen len buffer)
                     (dump writer buffer sec usec :length caplen :origlength len)
                     (write-image image-path
                                  buffer
                                  imagebuffer
                                  counter
                                  rotate-at
                                  snaplen)
                     (incf counter)
                     (format t "[~D] Packet length: ~A bytes, on the wire: ~A bytes~%" counter caplen len)))
          (sleep 0.1))))))
