(defpackage skel.cli
  (:use :cl :skel :macs.cli :sb-unicode)
  (:export :main))

(in-package :skel.cli)

(defun main ()
  (with-cli-handlers
      (progn
	(print (+ 2 2))
	(terpri)
	;; test C-c
	(sleep 4))))
