(defpackage skel
  (:use :cl :sxp :macs.fu)
  (:export :skel :def-skel-class
	   :skel-project))

(in-package :skel)

(defclass skel (sxp)
  ((id :initarg :id :initform nil))
  (:documentation "Base class for skeleton objects. Inherits from `sxp'."))

(defmacro def-sk-class (name doc &optional slots superclasses)
  "Define a new class with superclass of (`skel' . SUPERCLASSES), SLOTS, DOC, and NAME."
  `(defclass ,(symb 'sk- name)
       ,(if superclasses `(skel ,@superclasses) '(skel))
     ;; TODO 2023-08-26: 
     ,(if slots
	  `(,@slots
	    (id :initarg :id :initform nil))
	  `((id :initarg :id :initform nil)))
     (:documentation ,doc)))

(def-sk-class project "Project skeleton class.")
