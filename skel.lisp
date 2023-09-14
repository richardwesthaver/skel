;;; skel.lisp --- skeleton library
(eval-when (:compile-toplevel :load-toplevel :execute) (require 'sb-posix))

(defpackage skel
  (:use :cl :sxp :cond :fu :fmt :sb-mop :skel.make)
  (:import-from :sb-posix :getcwd :getuid)
  (:import-from :sb-unix :uid-username)
  (:shadowing-import-from :uiop :pathname-parent-directory-pathname :read-file-form)
  (:export
   :*skel-project* :*skel-project-registry* :*default-skelfile* :*default-skel-user* 
   :*default-skel-cache* :*default-user-skel-config* :*default-global-skel-config* :*skelfile-extension*
   :*skelfile-boundary* :find-skelfile :load-skelfile
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source :sk-recipe :sk-rule :sk-description
   :sk-kind :sk-rules :sk-id :sk-version :sk-name :sk-documents :sk-document
   :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :describe-skeleton :describe-project))

(in-package :skel)

;;; Vars
(deftype vc-designator () '(member :hg :git nil))

(declaim
 (type sk-project *skel-project*)
 (type string *default-skelfile* *default-skel-user* *skelfile-extension*)
 (type pathname *default-skel-cache* *default-user-skel-config* *default-global-skel-config*)
 (type vc-designator *default-skel-vc*))

(defvar *skel-project*)

;; TODO (defparameter *skel-project-registry* nil)

;; TODO (defvar *skelfile-boundary* nil "Set an upper bounds on how
;; many times and how far to walk an arbitrary file directory.")

(defparameter *default-skelfile* "skelfile")
(defparameter *default-skel-user* (uid-username (getuid)))
(defparameter *skelfile-extension* "sk")

(defparameter *default-skel-cache* (make-pathname :directory (format nil "home/~a/.cache/skel" *default-skel-user*)))
(defparameter *default-user-skel-config* (make-pathname :name (format nil "home/~a/.skelrc" *default-skel-user*)))
(defparameter *default-global-skel-config* (make-pathname :name "/etc/skelrc"))

(defparameter *default-skel-vc* :hg)

;;; Conditions
(define-condition skel-syntax-error (sxp-syntax-error) ())

(define-condition skel-fmt-error (sxp-fmt-error) ())

;;; Macros
(defmacro %def-sk-class (name doc superclasses slots)
  `(defclass ,(symb 'sk- name)
       ,superclasses
     ,slots
     (:documentation ,doc)))

(defmacro def-sk-class (name doc &optional superclasses slots)
  "Define a new class with superclass of (`skel' . SUPERCLASSES), SLOTS, DOC, and NAME."
  `(%def-sk-class
    ,name ,doc
    ,(if superclasses `(skel ,@superclasses) '(skel))
     ;; TODO 2023-08-26: 
    ,(when slots
	 `(,@slots))))

;;; Proto
(defgeneric sk-run (self))
(defgeneric sk-init (self))
(defgeneric sk-new (self))
(defgeneric sk-save (self))
(defgeneric sk-tangle (self))
(defgeneric sk-weave (self))
(defgeneric sk-call (self))
(defgeneric sk-print (self))
(defgeneric rehash-object (self))
(defgeneric init-skelfile (self &key path &allow-other-keys))
;;; Objects
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

(def-sk-class vc-meta
  "Skel Version Control systems."
  (sk-meta)
  ((vc :initarg :vc :initform *default-skel-vc* :accessor sk-vc)))

(%def-sk-class project
  "Skel Projects."
  (sk-vc-meta)
  ((rules :initarg :rules :initform nil :accessor sk-rules :type (or list (vector sk-rule)))
   (documents :initarg :documents :initform nil :accessor sk-documents :type (or list (vector sk-document)))
   (scripts :initarg :scripts :initform nil :accessor sk-scripts :type (or list (vector sk-script)))
   (snippets :initarg :snippets :initform nil :accessor sk-snippets :type (or list (vector sk-snippet)))
   (stash :initarg :stash :initform nil :accessor sk-stash :type (or null pathname))
   (shed :initarg :shed :initform nil :accessor sk-shed :type (or null pathname))
   (abbrevs :initarg :abbrevs :initform nil :accessor sk-abbrevs :type (or list (vector sk-abbrevs)))))

;; ast -> obj
(defmethod load-ast ((self sk-project))
  ;; internal ast is never tagged
  (with-slots (ast) self
    (if (formp ast)
	;; ast is valid, modify object, set ast nil
	(progn
	  (sb-int:doplist (k v) ast
	    (setf (slot-value self (intern (symbol-name k) :skel)) v))
	  (setf (ast self) nil)
	  self)
	;; invalid ast, signal error
	(error 'skel-syntax-error))))

;; obj -> ast
(defmethod build-ast ((self sk-project) &key (nullp nil))
  (setf (ast self) (unwrap-object self :slots t :methods nil :nullp nullp)))

(defmethod write-sxp-stream ((self sk-project) stream &key (pretty t) (case :downcase))
  (write (ast self) :stream stream :pretty pretty :case case))
  
(defun make-header-comment (name &key (timestamp nil) (description nil) (opts nil))
  (format nil ";;; ~A~A~A~A~%" name
	  (if timestamp
	      (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time) 0)
	      (format nil " @ ~4,'0d-~2,'0d-~2,'0d.~2,'0d:~2,'0d:~2,'0d" y mo d h m s))
	      "")
	  (if description
	      (format nil " --- ~A" description)
	      "")
	  (if opts
	      (format nil " -*- ~{~A~^;~} -*-" opts)
	      "")))

;; ast -> file
(defmethod init-skelfile ((self sk-project) &key (path *default-skelfile*) (nullp nil) (comment t))
  (with-slots (ast) self
    (build-ast self :nullp nullp)
    (prog1 
	(with-open-file (out path
			     :direction :output
			     :if-exists :error
			     :if-does-not-exist :create)
	  (when comment (princ
			 (make-header-comment
			  (sk-name self)
			  :timestamp t
			  :description (sk-description self)
			  :opts nil)
			 out))
	  (write-sxp-stream self out))
      (setf ast nil))))

;;; Debug
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

;;; Functions
(defun load-skelfile (file)
  "Load the 'skelfile' FILE."
  (let ((form (read-file-form file)))
    (load-ast (make-instance 'sk-project :ast form :id (sxhash form)))))

(defun find-skelfile (start &key (load nil) (name *default-skelfile*) (walk t))
  "Walk up the current directory returning the path to a 'skelfile', else
return nil. When LOAD is non-nil, load the skelfile if found."
  ;; Check the current path, if no skelfile found, walk up a level and
  ;; continue until the `*skelfile-boundary*' is triggered.
  (if walk 
      (let ((root (find-project-root (make-pathname :directory (pathname-directory start)) name)))
	(if root
	    (if load
		(load-skelfile (merge-pathnames name root))
		(merge-pathnames name root))
	    (warn "failed to find skelfile")))
      (if-let ((sk (probe-file (merge-pathnames name start))))
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
	(if (eql path next)
	    (find-project-root next name)
	    (warn "failed to find project root")))))
