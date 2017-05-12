(in-package :monsoon)

(defparameter *debug* nil)

(defparameter *highlight-ascii* t)

(defparameter *ascii-len* 4)

(defparameter *bitwise* nil)

(defparameter *interface* "wlp3s0")

(defparameter *promiscuous* t)

(defparameter *pcap-path* "/dev/null")

(defparameter *filter* "")

(defparameter *rate* 1)

(defparameter *tweakables*
  '(*interface*
    *debug*
    *ascii-len*
    *highlight-ascii*
    *filter*
    *rate*
    *pcap-path*
    *promiscuous*))
