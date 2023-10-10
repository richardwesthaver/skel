;;; cli.lisp --- skel cli

;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel :fmt :log :fu :readtables)
  (:import-from :sb-posix :getcwd)
  (:export :main))

(in-package :skel.cli)

(defun skc-build ())

(defcmd skc-init
    (let ((file (when $args (pop $args)))
	  (name (when (> $argc 1) (pop $args))))
      (init-skelfile file name)))

(defcmd skc-describe
    (describe 
     (find-skelfile 
      (if $args (make-pathname :defaults (car $args))
	  #P".")
      :load t)))

(defcmd skc-inspect
    (inspect
     (find-skelfile 
      (if $args (make-pathname :defaults (car $args))
	  #P".")
      :load t)))

(define-cli $cli
  :name "skel"
  :version "0.1.1"
  :description "A hacker's project compiler and build tool."
  :opts (make-opts 
	  (:name help :global t :description "print this message" 
	   :thunk (lambda (x) (when x (print-help $cli))))
	  (:name version :global t :description "print version" 
	   :thunk (lambda (x) (when x (print-version $cli))))
	  (:name debug :global t :description "set log level (debug,info,trace,warn)"
	   :thunk (lambda (x) (setq *log-level* (if x :debug *log-level*))))
	  (:name config :global t :description "set a custom skel user config" :kind file
	   :thunk (lambda (x) (init-skel-user-config (car x)))) ;; :kind?
	  (:name input :description "input source" :kind string)
	  (:name output :description "output target" :kind string))
  :cmds (make-cmds
	  (:name status
	   :description "print the status of the current project")
	  (:name init
	   :description "initialize a skelfile in the current directory"
	   :opts (make-opts 
		   (:name name :description "project name" :kind string))
	   :thunk skc-init)
	  (:name describe
	   :description "describe the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile" :kind file))
	   :thunk skc-describe)
	  (:name inspect
	   :description "describe the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile" :kind file))
	   :thunk skc-inspect)
	  (:name build
	   :description "build artifacts"
	   :opts (make-opts (:name target :description "target artifact to build" :kind string))
	   :thunk skc-build)
	  (:name make)
	  (:name push)
	  (:name pull)
	  (:name edit))
  (print-help $cli))

(defun run ()
  (with-cli (opts cmds) $cli
    (in-readtable *macs-readtable*)
    ;; should be called from do-cmd
    (loop for o across (active-opts $cli t)
	  do (do-opt o))
    (debug!
     (list 'ARGS (coerce (cli-cmd-args $cli) 'list))
     (list 'OPTS (coerce (active-opts $cli) 'list))
     (list 'CMDS (coerce (active-cmds $cli) 'list)))
    (loop for c across (active-cmds $cli)
	  do (do-cmd c))))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
