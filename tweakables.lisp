(in-package :monsoon)

(defparameter *debug* nil
  "Set to T to print verbose debugging output to the console.")

(defparameter *highlight-ascii* t
  "Set to T to highlight strings of ASCII characters, NIL otherwise.")

(defparameter *ascii-len* 4
  "Minimum length for a string of ASCII characters to be highlighted. Works like the -n parameter for the strings utility.")

(defparameter *bitwise* nil
  "Set to NIL to map pixels to bytes, T to map them to bits.")

(defparameter *interface* "wlp3s0"
  "Network interface to sniff on.")

(defparameter *promiscuous* t
  "Sniff promiscuously (T or nil).")

(defparameter *pcap-path* "/dev/null"
  "Path to write PCAP data to. Set to /dev/null to ignore.")

(defparameter *image-path* nil
  "Path to write PPM image to.")

(defparameter *filter* ""
  "PCAP filter. Uses TCPDUMP syntax.")

(defparameter *rate* 1
  "Monsoon will process *rate* packets per iteration of its main loop.")

(defparameter *width* nil
  "Default window/image width")

(defparameter *height* nil
  "Default window/image height")

(defparameter *tweakables*
  '(*interface*
    *debug*
    *ascii-len*
    *highlight-ascii*
    *filter*
    *rate*
    *pcap-path*
    *image-path*
    *promiscuous*))

(loop for symbol in '(*pcap-path*
                      *image-path*
                      *filter*)
      do
      (setf (get symbol 'string) t))
