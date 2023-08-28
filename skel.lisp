(eval-when (:compile-toplevel :load-toplevel :execute) (require 'sb-posix))

(defpackage skel
  (:use :cl :sxp :macs.fu :sb-mop)
  (:import-from :sb-posix :getcwd)
  (:export :skel :def-sk-class
	   :*sk-project* :*sk-project-registry* :*sk-cache*
	   :sk-project :sk-target :sk-source :sk-recipe :sk-rule
	   :sk-project-type :sk-project-rules :sk-id
	   :sk-document :sk-script :sk-config :sk-snippet :sk-abbrev
	   :describe-skeleton :describe-project :print-api))

(in-package :skel)

(defparameter *sk-project* nil)
(defparameter *sk-project-registry* nil)
(defparameter *sk-cache* nil)

(defclass skel (sxp)
  ((id :initarg :id :initform nil :accessor sk-id))
  (:documentation "Base class for skeleton objects. Inherits from `sxp'."))

(defmethod print-object ((self skel) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~{~a=~a~^, ~}"
	    (let (r)
	      (dolist (s (mapcar #'slot-definition-name (class-direct-slots (class-of self))) r)
		(if (slot-boundp self s)
		    (setf r (nconc r (list s (slot-value self s))))))
	      r))))


(defmacro def-sk-class (name doc &optional superclasses slots)
  "Define a new class with superclass of (`skel' . SUPERCLASSES), SLOTS, DOC, and NAME."
  `(defclass ,(symb 'sk- name)
       ,(if superclasses `(skel ,@superclasses) '(skel))
     ;; TODO 2023-08-26: 
     ,(if slots
	  `(,@slots
	    (id :initarg :id :initform nil))
	  `((id :initarg :id :initform nil)))
     (:documentation ,doc)))

(def-sk-class meta "Meta skeleton class." ()
  ((name :initarg :name :initform nil :type (or null string))
   (path :initarg :path :initform nil :type (or null pathname))
   (version :initarg :version :initform nil :type (or list string))
   (description :initarg :description :initform nil :type (or null string))))
(def-sk-class command "Command skeleton class.")
(def-sk-class target "Target skeleton class.")
(def-sk-class source "Source skeleton class.")
(def-sk-class recipe "Recipe skeleton class." ()
  ((commands :initarg :commands :initform nil :type (or list (vector sk-command)))))

(def-sk-class rule
  "Rule skeleton class. Maps a `sk-source' to a corresponding `sk-target'
via the special form stored in the `ast' slot."
  ()
  ((target :initarg :target :initform nil :type (or null sk-target))
   (source :initarg :source :initform nil :type (or null sk-source))
   (recipe :initarg :recipe :initform nil :type (or null sk-recipe))))
(def-sk-class document "Document skeleton class.")
(def-sk-class script "Document skeleton class.")
(def-sk-class config "Document skeleton class.")
(def-sk-class snippet "Document skeleton class.")
(def-sk-class abbrev "Document skeleton class.")
(def-sk-class project "Project skeleton class."
  ()
  ((type :initarg :type :initform nil :accessor sk-project-type)
   (rules :initarg :rules :initform nil :accessor sk-project-rules :type (or list (vector sk-rule)))
   (documents :initarg :documents :initform nil :accessor sk-project-documents :type (or list (vector sk-document)))
   (scripts :initarg :scripts :initform nil :accessor sk-project-scripts :type (or list (vector sk-script)))
   (snippets :initarg :snippets :initform nil :accessor sk-project-snippets :type (or list (vector sk-snippet)))
   (abbrevs :initarg :abbrevs :initform nil :accessor sk-project-abbrevs :type (or list (vector sk-abbrevs)))))

;;; util
(defun describe-skeleton (skel &optional (stream t))
  "Describe the object SKEL which should inherit from the `skel' superclass."
  (format stream "~A~%" skel)
  (terpri stream))

(defun describe-project (&optional path (stream t))
  "Describe the project responsible for the pathname PATH. Defaults to
`sb-posix:getcwd'."
  (let ((project-path (or path (getcwd))))
    (print project-path stream)
    (terpri stream)))

(defun print-api (&optional root)
  "Print a tree of CLOS classes and methods to *standard-output*."
  (declare (ignorable root))
  (mapc
   (lambda (sk) (describe-skeleton (make-instance sk)))
   '(skel sk-project sk-source sk-target sk-recipe sk-rule
     sk-document sk-script sk-config sk-snippet sk-abbrev)))
