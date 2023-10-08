;;; Utils
(in-package :skel)

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
    (sk-write-file sk :path path :fmt fmt :if-exists)))

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

