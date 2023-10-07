;;; cli.lisp --- skel cli

;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel :fmt :log :fu)
  (:import-from :sb-posix :getcwd)
  (:export :main))

(in-package :skel.cli)

(define-cli $cli
  :name "skel"
  :version "0.1.1"
  :description "A hacker's project compiler and build tool."
  :opts (make-opts 
	  (:name help :global t :description "print this message" :thunk (lambda (x) (declare (ignore x)) (print-help $cli)))
	  (:name version :global t :description "print version" :thunk (lambda (x) (declare (ignore x)) (print-version $cli)))
	  (:name debug 
	   :global t :description "set log level (debug,info,trace,warn)"
	   :thunk (lambda (x) (or (when x :debug) *log-level*)))
	  (:name input :description "input source")
	  (:name output :description "output target"))
  :cmds (make-cmds
	  (:name status
	   :description "print the status of the current project")
	  (:name init
	   :description "initialize a skelfile in the current directory"
	   :opts (make-opts 
		   (:name name :description "project name")
		   (:name kind :description "project kind")))
	  (:name describe
	   :description "describe the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile"))
	   :thunk (lambda (x) (declare (ignore x)) (describe (find-skelfile #P"." :load t))))
	  (:name build
	   :description "build artifacts"
	   :opts (make-opts (:name target :description "target artifact to build")))
	  (:name run)))

(defun run ()
  (with-cli (opts cmds) $cli
    (loop for o across (active-opts $cli t)
	  if (string= "debug" (cli-name o))
	    do (setq *log-level* (do-opt o))
	  else do (do-opt o))
    (debug! (cli-opts $cli) (cli-cmd-args $cli) (cli-cmds $cli))
    (loop for c across (active-cmds $cli)
	  if (string= "describe" (cli-name c))
	    do (do-cmd c)
	  else do (iprintln ":OK"))))
       
(defmain ()
  (run)
  (sb-ext:exit :code 0))
