;;; skel.lisp --- skeleton library

;; A hacker's project compiler.

;;; Commentary:

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute) (require 'sb-posix))

(defpackage skel
  (:use :cl :sxp :cond :fu :fmt :sb-mop :log)
  (:import-from :sb-posix :getcwd :getuid)
  (:import-from :sb-unix :uid-username)
  (:shadowing-import-from :uiop :pathname-parent-directory-pathname :read-file-forms)
  (:export
   :*skel-project* :*skel-project-registry* :*default-skelfile* :*default-skel-user* 
   :*default-skel-cache* :*default-user-skel-config* :*default-global-skel-config* :*skelfile-extension*
   :make-file-header :make-shebang-file-header :make-source-file-header :file-header-kind
   :make-source-header-comment :make-shebang-comment :*skelfile-boundary* :find-skelfile :load-skelfile
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source :sk-recipe :sk-rule :sk-description
   :sk-kind :sk-rules :sk-id :sk-version :sk-name :sk-documents :sk-document :sk-command
   :sk-compile
   :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :describe-skeleton :describe-project :init-skelfile
   :sk-write-file :sk-read-file
   :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push))

(in-package :skel)

;;; Vars
(deftype vc-designator () '(member :hg :git nil))

(declaim
 (type sk-project *skel-project*)
 (type string *default-skel-user* *default-skelfile* *skelfile-extension*)
 (type pathname *default-skel-cache* *default-user-skel-config* *default-global-skel-config*)
 (type vc-designator *default-skel-vc-kind*))

(defvar *skel-project*)

;; TODO (defparameter *skel-project-registry* nil)

;; TODO (defvar *skelfile-boundary* nil "Set an upper bounds on how
;; many times and how far to walk an arbitrary file directory.")

(defparameter *default-skel-user* (uid-username (getuid)))

(defparameter *default-skelfile* "skelfile")
(defparameter *skelfile-extension* "sk")

(defparameter *default-skel-cache* (make-pathname :directory (format nil "home/~a/.cache/skel" *default-skel-user*)))
(defparameter *default-user-skel-config* (make-pathname :name (format nil "home/~a/.skelrc" *default-skel-user*)))
(defparameter *default-global-skel-config* (make-pathname :name "/etc/skelrc"))

(defparameter *default-skel-vc-kind* :hg)

;;; Conditions
(define-condition skel-syntax-error (sxp-syntax-error) ())

(define-condition skel-fmt-error (sxp-fmt-error) ())

;;; Proto
(defgeneric sk-run (self))
(defgeneric sk-new (self))
(defgeneric sk-save (self))
(defgeneric sk-tangle (self))
(defgeneric sk-weave (self))
(defgeneric sk-call (self))
(defgeneric sk-print (self))
(defgeneric sk-load (self &key &allow-other-keys))
(defgeneric sk-compile (self stream &key &allow-other-keys))
(defgeneric rehash-object (self))
(defgeneric sk-transform (self other &key &allow-other-keys))
;; TODO 2023-09-22: consider a skelfile-writer struct
(defgeneric sk-read-file (self &key path &allow-other-keys))
(defgeneric sk-write-file (self &key path &allow-other-keys))
			   
;;; Objects
(defclass skel ()
  ((id :initarg :id :initform (sxhash nil) :accessor sk-id :type fixnum))
  (:documentation "Base class for skeleton objects. Inherits from `sxp'."))

(defmethod print-object ((self skel) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S ~A" :id (fmt-sxhash (sk-id self)))))

(defmethod initialize-instance :before ((self skel) &rest initargs &key &allow-other-keys)
  (unless (getf initargs :id)
    ;; TODO 2023-09-10: make fast 
    (with-slots (id) self
      (setf id (sxhash self)))
  (when (next-method-p)
    (call-next-method))))

;; TODO 2023-09-11: research other hashing strategies - maybe use the
;; sxhash as a nonce for UUID
(defmethod rehash-object ((self skel))
  (setf (sk-id self) (sxhash self)))

;; note that the sk-meta class does not inherit from skel or sxp.
(defclass sk-meta ()
  ((name :initarg :name :initform nil :type (or null string) :accessor sk-name)
   (path :initarg :path :initform nil :type (or null pathname) :accessor sk-path)
   (author :initarg :author :type string :accessor sk-author)
   (version :initarg :version :type string :accessor sk-version)
   (kind :initarg :kind :accessor sk-kind)
   (description :initarg :description :initform nil :type (or null string) :accessor sk-description)
   (license :initarg :license :type :string :accessor sk-license))
  (:documentation "Skel Meta class."))
 
(defun sk-init (class &rest initargs)
  (apply #'make-instance class initargs))

(defmacro sk-init-dir (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (sk-path self) (getcwd)))
     self))

(defmacro sk-init-file (class &rest initargs)
  `(let ((self (sk-init ',class ,@initargs)))
     (unless (getf ',initargs :path)
       (setf (sk-path self) *default-skelfile*))
     self))

(defclass sk-command (skel)
  ())

(defclass sk-target (skel)
  ())

(defclass sk-source (skel)
  ())

(defclass sk-recipe (skel)
  ((body :initarg :commands :initform nil :type list :accessor sk-body)))

(defclass sk-rule (skel)
  ((target :initarg :target :type sk-target)
   (source :initarg :source :type sk-source)
   (recipe :initarg :recipe :type sk-recipe))
  (:documentation "Skel rules. Maps a `sk-source' to a corresponding `sk-target'
via the special form stored in the `ast' slot."))

(defmacro sk-make-rule (target source &body recipe)
  "Make a new SK-RULE."
  `(let ((r (make-instance 'sk-recipe :body ',recipe)))
     (make-instance 'sk-rule :target ,target :source ,source :recipe r)))

(defclass sk-document (skel sk-meta sxp)
  ())

(defclass sk-script (skel sk-meta sxp)
  ())

(defclass sk-config (skel sk-meta sxp) ())

(defclass sk-user-config (sk-config)
  ((default-fmt :type symbol)
   ;; TODO 2023-09-26: can change type to vc-meta, use as a base
   ;; template for stuff like pre-defined remote URLs.
   (default-vc :type vc-designator)
   (default-shed :type string)
   (default-stash :type string)
   (default-license :type string)
   (default-log-level :type log-level-designator)
   (user :type form)
   (auto-insert :type form)
   (custom :type form)))

(defstruct sk-snippet ""
  (name "" :type string) 
  (form "" :type form))

(defstruct sk-abbrev ""
  (match nil :type form) 
  (expansion nil :type form))

(defstruct sk-vc-remote-meta ""
	   (name :default :type keyword)
	   (path nil :type (or symbol string)))

(defmethod write-sxp-stream ((self sk-vc-remote-meta) stream &key (pretty t) (case :downcase) (fmt :collapsed))
  (declare (ignore fmt))
  (write `(,(sk-vc-remote-meta-name self) ,(sk-vc-remote-meta-path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defstruct sk-vc-meta ""
	   (kind *default-skel-vc-kind* :type vc-designator)
	   (remotes nil :type list))

(defmethod write-sxp-stream ((self sk-vc-meta) stream &key (pretty t) (case :downcase) (fmt :collapsed))
  (if (= 0 (length (sk-vc-meta-remotes self)))
      (write (sk-vc-meta-kind self) :stream stream :pretty pretty :case case :readably t :array t :escape t)
      (progn
	(format stream "(")
	(write (sk-vc-meta-kind self) :stream stream :pretty pretty :case case :readably t :array t :escape t)      
	(format stream " ")
	(loop for x in (sk-vc-meta-remotes self)
	      do 
		 (write-sxp-stream x stream :pretty pretty :case case :fmt fmt))
	(format stream ")"))))
  
(defclass sk-project (skel sxp sk-meta)
  ((name :initarg :name :initform "" :type string)
   (vc :initarg :vc :initform (make-sk-vc-meta :kind *default-skel-vc-kind*) :type sk-vc-meta :accessor sk-vc)
   (rules :initarg :rules :initform nil :accessor sk-rules :type (or list (vector sk-rule)))
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
(defmethod build-ast ((self sk-project) &key (nullp nil) (exclude '(ast id)))
  (setf (ast self)
	 (unwrap-object self
			:slots t
			:methods nil
			:nullp nullp
			:exclude exclude)))

;; TODO 2023-09-26: This belongs in sxp
(defmethod write-sxp-stream ((self sk-project) stream &key (pretty t) (case :downcase) (fmt :collapsed))
  (case fmt
    (:collapsed
     (if (listp (ast self))
	 (loop for (k v . rest) on (ast self)
	       by #'cddr
	       unless (or (null v) (null k))
		 do 
		    (write k :stream stream :pretty pretty :case case :readably t :array t :escape t)
		    (format stream " ")
		    (if (or (eq (type-of v) 'skel) (subtypep (type-of v) 'structure-object))
			(write-sxp-stream v stream :pretty pretty :case case)
			(write v :stream stream :pretty pretty :case case :readably t :array t :escape t))
		    (format stream "~%"))
	 (error 'sxp-fmt-error)))
    (t (write (ast self) :stream stream :pretty pretty :case case :readably t :array t :escape t))))

;; file -> ast
;; allow-comment?
(defmethod sk-read-file ((self sk-project) &key (path *default-skelfile*))
  (wrap self (read-file-forms path))
  self)

;; ast -> file
(defmethod sk-write-file ((self sk-project) 
			  &key 
			    (path *default-skelfile*) (nullp nil) (comment t) (fmt :canonical)
			    (if-exists :error))
    (build-ast self :nullp nullp)
  (prog1 
      (with-open-file (out path
			   :direction :output
			   :if-exists if-exists
			   :if-does-not-exist :create)
	(when comment (princ
		       (make-source-header-comment
			(sk-name self)
			:cchar #\;
			:timestamp t
			:description (sk-description self)
			:opts '("mode: skel;"))
		       out))
	(write-sxp-stream self out :fmt fmt))
    (setf (ast self) nil)))

;;; File Headers
(deftype file-header-kind () '(member :source :shebang))

(declaim (inline %make-file-header))
(defstruct (file-header (:constructor %make-file-header)
			   (:conc-name sk-fh-))
  (kind :source :type file-header-kind)
  (str "" :type string))

(defun make-file-header (kind string)
  (%make-file-header :kind kind :str (or string "")))

(defun make-source-file-header (str)
  (make-file-header :source str))

(defun make-shebang-file-header (str)
  (make-file-header :shebang str))

;; TODO 2023-09-17: this should be a struct I think - file-header maybe?
(defun make-source-header-comment (name &key (cchar #\;) (timestamp nil) (description nil) (opts nil))
  "Generate a generic file-header with optional timestamp, description, and opts."
  (format nil "~A ~A~A~A~A~%" (make-string 3 :initial-element cchar) 
	  name
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

(defun make-shebang-comment (shell &rest args)
  "Generate a shebang file-header line."
  (format nil "#~A ~{~A~^ ~}~%" shell args))

;;; Utils
(defun load-skelfile (file)
  "Load the 'skelfile' FILE."
  (let ((form (read-file-forms file)))
    (when (= 1 (length form)) ;; a single form - unwrap it
      (setq form (car form)))
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

(defun init-skelfile (&optional file name fmt)
  (let ((sk (make-instance 'sk-project :name (or name (pathname-name (getcwd)))))
	(path (or file *default-skelfile*)))
    (sk-write-file sk :path path :fmt fmt)))

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

;;; VM

;; Stack slots refer to objects. a Stack is a sequence of objects
;; which can be output to a stream using a specialized function.

(deftype stack-slot-kind () '(member :shell :lisp :comment :var :rule :directive :nop))

(defstruct stack-slot (kind :nop :type stack-slot-kind) (spec nil :type form) (form nil :type form))
  
(declaim (inline %make-sk-vm))
(defstruct (sk-vm (:constructor %make-sk-vm))
  ;; TODO 2023-09-23: consider making this an open closure, call it in
  ;; MAKE-SK-VM.
  (ip (make-stack-slot) :type stack-slot)
  (stack (make-array 0) :type (array stack-slot)))

(defun make-sk-vm (size) 
  (let ((vm (%make-sk-vm :stack (make-array size :fill-pointer t :initial-element (make-stack-slot)))))
    (with-slots (ip stack) vm
      (setf ip (aref stack 0))
    vm)))

(defmethod sks-ref ((vm sk-vm)) (setf (sk-vm-ip vm) (aref (sk-vm-stack vm) 0)))

(defmethod sks-pop ((vm sk-vm)) (setf (sk-vm-ip vm) (vector-pop (sk-vm-stack vm))))

(defmethod sks-push ((slot stack-slot) (vm sk-vm)) (vector-push slot (sk-vm-stack vm)))
