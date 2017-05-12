(load "~/quicklisp/setup.lisp")
(ql:quickload :monsoon)
(sb-ext:save-lisp-and-die "build/monsoon"
                          :executable t
                          :toplevel #'monsoon::main)
