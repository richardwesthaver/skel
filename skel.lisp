;;; skel.lisp --- skeletons library
(eval-when (:compile-toplevel :load-toplevel :execute) (require 'sb-posix))

(defpackage skel
  (:use :cl :sxp :cond :fu :fmt :sb-mop :skel.make)
  (:import-from :sb-posix :getcwd :getuid)
  (:import-from :sb-unix :uid-username)
  (:import-from :uiop :pathname-parent-directory-pathname)
  (:export
   :*skel-project* :*skel-project-registry* :*default-skelfile* :*default-skel-user* 
   :*default-skel-cache* :*default-user-skel-config* :*default-global-skel-config* :*skelfile-extension*
   :*skelfile-boundary* :find-skelfile :load-skelfile
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source :sk-recipe :sk-rule :sk-description
   :sk-kind :sk-rules :sk-id :sk-version :sk-name :sk-documents :sk-document
   :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :describe-skeleton :describe-project))

(in-package :skel)

;;; VARS
(defparameter *skel-project* nil)
(defparameter *skel-project-registry* nil)
(defparameter *default-skelfile* "skelfile")
(defparameter *default-skel-user* (uid-username (getuid)))
(defparameter *default-skel-cache* (make-pathname :directory (format nil "home/~a/.cache/skel" *default-skel-user*)))
(defparameter *default-user-skel-config* (make-pathname :name (format nil "home/~a/.skelrc" *default-skel-user*)))
(defparameter *default-global-skel-config* (make-pathname :name "/etc/skelrc"))
(defparameter *skelfile-extension* "sk")
(defvar *skelfile-boundary* nil "Set an upper bounds on how many times and how far to walk an arbitrary
file directory.")

;;; COND
(define-condition skel-syntax-error (sxp-syntax-error) ())

(define-condition skel-fmt-error (sxp-fmt-error) ())

;;; UTIL
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

;;; PROTO

(defgeneric sk-run (self))
(defgeneric sk-init (self))
(defgeneric sk-new (self))
(defgeneric sk-save (self))
(defgeneric sk-tangle (self))
(defgeneric sk-weave (self))
(defgeneric sk-call (self))
(defgeneric sk-print (self))
(defgeneric rehash-object (self))
;;; OBJ
(defclass skel (sxp)
  ((id :initarg :id :initform (required-argument :id) :accessor sk-id :type fixnum))
  (:documentation "Base class for skeleton objects. Inherits from `sxp'."))

(defmethod print-object ((self skel) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (fmt-sxhash (sk-id self)))))

(defmethod initialize-instance ((self skel) &rest initargs &key &allow-other-keys)
  (unless (getf initargs :id)
    ;; TODO 2023-09-10: make fast 
    (setf (sk-id self)
	  (sxhash self)))
  (unless (getf initargs :path)
    (setf (sk-path self) (getcwd)))
  (call-next-method))

;; TODO 2023-09-11: research other hashing strategies - maybe use the
;; sxhash as a nonce for UUID
(defmethod rehash-object ((self skel))
  (setf (sk-id self) (sxhash self)))

(defclass sk-meta ()
  ((name :initarg :name :initform nil :type (or null string) :accessor sk-name)
   (path :initarg :path :initform nil :type (or null pathname) :accessor sk-path)
   (author :initarg :author :type string :accessor sk-author)
   (version :initarg :version :type string :accessor sk-version)
   (kind :initarg :kind :accessor sk-kind)
   (description :initarg :description :initform nil :type (or null string) :accessor sk-description)
   (license :initarg :license :type :string :accessor sk-license))
  (:documentation "Skel Meta class."))
   
(def-sk-class command
  "Skel commands."
  ())

(def-sk-class target
  "Target skeleton class."
  ())

(def-sk-class source
  "Skel sources."
  ())

(def-sk-class recipe
  "Skel recipes."
  ()
  ((commands :initarg :commands :initform nil :type (or list (vector sk-command)))))

(def-sk-class rule
  "Skel rules. Maps a `sk-source' to a corresponding `sk-target'
via the special form stored in the `ast' slot."
  ()
  ((target :initarg :target :initform nil :type (or null sk-target))
   (source :initarg :source :initform nil :type (or null sk-source))
   (recipe :initarg :recipe :initform nil :type (or null sk-recipe))))

(def-sk-class document
  "Skel Documents."
  ())

(def-sk-class script
  "Skel Scripts."
  ())

(def-sk-class config
  "Skel Configs."
  ())

(def-sk-class snippet
  "Skel Snippets."
  ())

(def-sk-class abbrev
  "Skel Abbrevs."
  ())

(def-sk-class project
  "Skel Projects."
  (sk-meta)
  ((rules :initarg :rules :initform nil :accessor sk-rules :type (or list (vector sk-rule)))
   (documents :initarg :documents :initform nil :accessor sk-documents :type (or list (vector sk-document)))
   (scripts :initarg :scripts :initform nil :accessor sk-scripts :type (or list (vector sk-script)))
   (snippets :initarg :snippets :initform nil :accessor sk-snippets :type (or list (vector sk-snippet)))
   (abbrevs :initarg :abbrevs :initform nil :accessor sk-abbrevs :type (or list (vector sk-abbrevs)))))

;; ast -> obj
(defmethod load-ast ((self sk-project))
  ;; internal ast is never tagged
  (with-slots (ast) self
    (if (formp ast)
	;; ast is valid, modify object, set ast nil
	(progn
	  (sb-int:doplist (k v) ast
	    (setf (slot-value self (symb k)) v))
	  (setf (ast self) nil)
	  self)
	;; invalid ast, signal error
	(error 'skel-syntax-error))))

;; obj -> ast
(defmethod build-ast ((self sk-project))
  (setf (ast self) (unwrap-object self :methods nil)))

;;; DBG
(defun describe-skeleton (skel &optional (stream t))
  "Describe the object SKEL which should inherit from the `skel' superclass."
  (print-object skel stream)
  (terpri stream))

(defun describe-project (&optional path (stream t))
  "Describe the project responsible for the pathname PATH. Defaults to
`sb-posix:getcwd'."
  (let* ((cd (or path (getcwd))))
    (print cd stream)
    (terpri stream)))

;;; Skelfile
(defun init-skelfile (path)
  (with-open-file (out path
		       :direction :output
		       :if-exists :error
		       :if-does-not-exist :create)
    (let ((obj (make-instance 'sk-project)))
      (write-sxp-stream (build-ast obj) out))))

(defun load-skelfile (file)
  "Load the 'skelfile' FILE."
  (let ((form (read-file-form file)))
    (load-ast (make-instance 'sk-project :ast form :id (sxhash form)))))

(defun find-skelfile (&key (path (getcwd)) (load nil) (name *default-skelfile*) (walk t))
  "Walk up the current directory returning the path to a 'skelfile', else
return nil. When LOAD is non-nil, load the skelfile if found."
  ;; Check the current path, if no skelfile found, walk up a level and
  ;; continue until the `*skelfile-boundary*' is triggered.
  (if walk 
      (let ((root (find-project-root path name)))
	(if root
	    (if load
		(load-skelfile (merge-pathnames name root))
		(merge-pathnames name root))
	    (warn "failed to find skelfile")))
      (if-let ((sk (probe-file (merge-pathnames name path))))
	(if load 
	    (load-skelfile sk)
	    sk)
	(warn "failed to find skelfile"))))

(defun find-project-root (path name)
  "Return PATH if it is a `skel-project' by checking for
  NAME."
  (if (probe-file (merge-pathnames name path))
      path
      (let ((next (pathname-parent-directory-pathname path)))
	(when next 
	  (find-project-root next name)))))
