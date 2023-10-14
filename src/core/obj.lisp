;;; Objects
(in-package :skel)

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
   (tags :initarg :tags :accessor sk-tags)
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
  ((body :initform nil :initarg :body :type form :accessor sk-body)))

(defmethod sk-write ((self sk-command) stream)
  (if (stringp (sk-body self)) (format stream "~A" (sk-body self))))

(defmethod sk-write-string ((self sk-command))
  (with-output-to-string (s)
    (sk-write self s)))
    
(defmethod sk-writeln ((self sk-command) stream) 
  (sk-write self stream)
  (format stream "~%"))

;;  HACK 2023-09-27: (defstruct sk-url) ?

(defclass sk-source (skel)
  ((path :initform "" :initarg :path :type string :accessor sk-path)))

(defmethod sk-write ((self sk-source) stream)
  (if (stringp (sk-path self)) (format stream "~A" (sk-path self))))

(defmethod sk-write-string ((self sk-source))
  (with-output-to-string (s)
    (sk-write self s)))

(defclass sk-rule (skel)
  ;; if target is a symbol, treated as a PHONY.
  ((target :initarg :target :type (or string symbol) :accessor sk-rule-target)
   (source :initarg :source :type (or sk-source null) :accessor sk-rule-source)
   (recipe :initform (make-instance 'sk-command) :initarg :recipe :type sk-command :accessor sk-rule-recipe))
  (:documentation "Skel rules. Maps a SOURCE to a corresponding TARGET
via the special form stored in RECIPE."))

(defmethod write-sxp-stream ((self sk-rule) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-rule-target self) ,(sk-rule-source self) ,@(sk-body (sk-rule-recipe self))) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmacro make-sk-rule (target source &body recipe)
  "Make a new SK-RULE."
  `(let ((r (make-instance 'sk-command :body ,recipe)))
     (make-instance 'sk-rule :target ,target :source ,source :recipe r)))

(defmethod sk-write ((self sk-rule) stream)
  (with-slots (target source recipe) self
    (sk-write-string target)
    (sk-write-string source)
    (sk-write-string recipe)))

(deftype document-designator () '(member :org :txt :pdf :html :md))

;; TODO 2023-10-13: integrate organ for working with org document
;; types - mixins and such
(defclass sk-document (skel sk-meta sxp)
  ((kind :initarg :kind :type document-designator)
   (export :initarg :setup :type form
	   :documentation "document export options")
   (attach :initarg :attach :type form
	   :documentation "document attachments"))
  (:documentation "Document object."))

(defclass sk-script (skel sk-meta sxp)
  ())

(defclass sk-config (skel sxp) nil)

(defclass sk-user-config (sk-config sk-meta)
  ((fmt :type symbol)
   ;; TODO 2023-09-26: can change type to vc-meta, use as a base
   ;; template for stuff like pre-defined remote URLs.
   (vc :type vc-designator)
   (shed :type string)
   (stash :type string)
   (license :type string)
   (log-level :type log-level-designator)
   (user :type form)
   (alias-list :type form
	       :documentation "alist of aliases. currently used as a special cli-opt-parser by the skel binary.")
   (auto-insert :type form))
  (:documentation "User configuration object, typically written to ~/.skelrc."))

;; ast -> obj
(defmethod load-ast ((self sk-user-config))
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

(defstruct sk-snippet
  (name "" :type string)
  (form "" :type form))

(defstruct sk-abbrev
  (match nil :type form) 
  (expansion nil :type form))

(defstruct sk-vc-remote-meta
  (name :default :type keyword)
  (path nil :type (or symbol string)))

(defmethod write-sxp-stream ((self sk-vc-remote-meta) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-vc-remote-meta-name self) ,(sk-vc-remote-meta-path self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defstruct sk-vc-meta 
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
   (components :initarg :components :initform nil :accessor sk-components :type list)
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

(declaim (inline file-read-forms))
(defun file-read-forms (file)
  (let ((form (read-file-forms file)))
    (if (cdr form)
	form
	(car form))))

;; file -> ast
(defmethod sk-read-file ((self sk-project) path)
  (wrap self (file-read-forms path))
  (setf (sk-path self) (ensure-absolute-pathname path *default-pathname-defaults*))
  self)

;; ast -> file
(defmethod sk-write-file ((self sk-project) 
			  &key 
			    (path *default-skelfile*) (nullp nil) (header t) (fmt :canonical)
			    (if-exists :error))
    (build-ast self :nullp nullp)
  (prog1 
      (with-open-file (out path
			   :direction :output
			   :if-exists if-exists
			   :if-does-not-exist :create)
	(when header (princ
		       (make-source-header-comment
			(sk-name self)
			:cchar #\;
			:timestamp t
			:description (sk-description self)
			:opts '("mode: skel;"))
		       out))
	(write-sxp-stream self out :fmt fmt))
    (setf (ast self) nil)))

(defmethod sk-install-user-config ((self sk-project) (cfg sk-user-config))
  (with-slots (vc shed stash license author) (debug! cfg) ;; log-level, custom, fmt
    (when vc (setf (sk-vc self) vc))
    (when shed (setf (sk-shed self) shed))
    (when license (setf (sk-license self) license))
    (when author (setf (sk-author self) author))))
