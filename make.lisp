;;; make.lisp --- GNU Makefile support for Skel

;; This package contains a GNU Makefile compiler and classes.

;;; Commentary:

;; https://github.com/takagi/lake

;;; Code:
(defpackage :skel.make
  (:use :cl :sxp)
  (:export :sk-make))

(in-package :skel.make)
