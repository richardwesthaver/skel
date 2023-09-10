;;; asdf.lisp --- ASDF system support for Skel

;; This package contains middleware between ASDF and Skel.

;;; Code:
(defpackage :skel.asdf
  (:use :cl :sxp)
  (:export :sk-asdf))

(in-package :skel.asdf)

