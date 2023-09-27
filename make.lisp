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
  (:use :cl :skel :fmt)
  (:export :makefile :push-rule :push-directive :push-var))

(in-package :skel.make)

(defparameter *default-makefile* "makefile")
(defparameter *makefile-extension* "mk")

;; https://www.gnu.org/software/make/manual/html_node/Makefile-Contents.html
(defclass makefile (skel sk-meta)
  ((directives :initform (make-array 0 :adjustable t :fill-pointer 0) :type (vector sk-command) :accessor mk-directives)
   (variables :initform (make-hash-table) :type (hash-table) :accessor mk-vars)
   (explicit :initform (make-array 0 :adjustable t :fill-pointer 0) :type (vector sk-rule) :accessor mk-erules)
   (implicit :initform (make-array 0 :adjustable t :fill-pointer 0) :type (vector sk-rule) :accessor mk-irules))
  (:documentation "A virtual GNU Makefile."))

(defmethod push-rule ((self sk-rule) (place makefile) &optional implicit)
  (if implicit
      (vector-push-extend self (mk-irules place))
      (vector-push-extend self (mk-erules place))))

(defmethod push-directive ((self sk-command) (place makefile))
  (vector-push-extend self (mk-directives place)))

(defmethod push-var ((self cons) (place makefile))
  (destructuring-bind (k v) self
    (setf (gethash k (mk-vars place)) v)))

(defmethod sk-compile ((self makefile) stream &key &allow-other-keys)
  "Compile the makefile SELF to output STREAM."
  (with-open-stream (s stream)
    (with-slots (directives variables explicit implicit) self
      ;; directives
      (loop for d across directives
	    do (format s "~A~%" d))
      ;; variables
      (maphash (lambda (x y) (format s "~A=~A~%" x y)) variables)
      ;; explicit rules
      (loop for exp across explicit
	    do (format s "~A:~A;~A~%" exp nil t))
      ;; TODO implicit rules
      (loop for imp across implicit
	    do (format s "~A:~A;~A~%" imp nil t)))))

(defmethod sk-write-file ((self makefile) &key (path *default-makefile*) (comment t) (if-exists :overwrite))
  (with-open-file (out path
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create)
    (when comment (princ
		   (make-source-header-comment
		    (sk-name self)
		    :cchar #\#
		    :timestamp t
		    :description (sk-description self)
		    :opts '("mode: makefile;"))
		   out))
    (sk-compile self out)))

(defmethod sk-read-file ((self makefile) &key (path *default-makefile*)))
