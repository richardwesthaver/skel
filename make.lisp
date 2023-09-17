;;; make.lisp --- GNU Makefile support for Skel

;; GNU Makefile compiler.

;;; Commentary:

;; Makefiles are our 'portable' build medium. We can parse them using
;; the same general strategy as GNU make and compile them from
;; skelfiles (rule, source, target, command).

;;  HACK 2023-09-15: MVP

;; SO, the absolute priority ATM is to transpile our `sk-rule' objects
;; into a working Makefile. We're ignoring most of the niceties like
;; line-splitting and any JIT or compile-time execution.

;; https://github.com/takagi/lake

;; https://www.gnu.org/software/make/manual/html_node/Parsing-Makefiles.html

;;; Code:
(defpackage :skel.make
  (:use :cl :skel)
  (:export :makefile))

(in-package :skel.make)

(defparameter *default-makefile* "makefile")
(defparameter *makefile-extension* "mk")

;; https://www.gnu.org/software/make/manual/html_node/Makefile-Contents.html
(defclass makefile (sk-meta)
  ((explicit :type (vector sk-rules) :accessor mk-erules)
   (implicit :type (vector sk-rules) :accessor mk-irules)
   (variables :type (hash-table) :accessor mk-vars)
   (directives :type (vector sk-command) :accessor mk-directives))
  (:documentation "A virtual GNU Makefile."))

(defmethod push-rule ((self makefile) (rule sk-rule) &optional implicit)
  (if implicit
      (vector-push rule (mk-erules self))
      (vector-push rule (mk-irules self))))

(defmethod push-directive ((self makefile) directive)
  (vector-push directive (mk-directives self)))

(defmethod push-var ((self makefile) var)
  (destructuring-bind (k v) var
    (setf (gethash k (mk-directives self)) v)))
