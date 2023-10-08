;;; cli.lisp --- skel cli

;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel :fmt :log :fu)
  (:import-from :sb-posix :getcwd)
  (:export :main))

(in-package :skel.cli)

(defun skc-build ())

(defcmd skc-init
    (iprintln $args)
  (apply #'init-skelfile $args))

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
	  (:name help :global t :description "print this message" :thunk (lambda (x) (when x (print-help $cli))))
	  (:name version :global t :description "print version" :thunk (lambda (x) (when x (print-version $cli))))
	  (:name debug 
	   :global t :description "set log level (debug,info,trace,warn)"
	   :thunk (lambda (x) (setq *log-level* (if x :debug *log-level*))))
	  (:name input :description "input source")
	  (:name output :description "output target"))
  :cmds (make-cmds
	  (:name status
	   :description "print the status of the current project")
	  (:name init
	   :description "initialize a skelfile in the current directory"
	   :opts (make-opts 
		   (:name name :description "project name"))
	   :thunk skc-init)
	  (:name describe
	   :description "describe the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile"))
	   :thunk skc-describe)
	  (:name inspect
	   :description "describe the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile"))
	   :thunk skc-inspect)
	  (:name build
	   :description "build artifacts"
	   :opts (make-opts (:name target :description "target artifact to build"))
	   :thunk skc-build)
	  (:name make)
	  (:name push)
	  (:name pull)
	  (:name edit))
  (print t))

(defun run ()
  (with-cli (opts cmds) $cli
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
