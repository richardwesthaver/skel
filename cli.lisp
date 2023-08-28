(defpackage skel.cli
  (:use :cl :skel :macs.cli)
  (:export :main))

(in-package :skel.cli)

(defmain
  (print (+ 2 2))
  (terpri)
  ;; test C-c
  (sleep 4))
