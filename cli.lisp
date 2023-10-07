;;; cli.lisp --- skel cli

;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel :fmt :log)
  (:import-from :sb-posix :getcwd)
  (:export :main))

(in-package :skel.cli)

(define-cli $cli
  :name "skel"
  :version "0.1.1"
  :description "A hacker's project compiler and build tool."
  :opts (make-opts 
	  (:name help :description "print this message")
	  (:name version :description "print version")
	  (:name debug 
	   :global t :description "set log level (debug,info,trace,warn)"
	   :thunk (lambda (&optional x) (setq *log-level* x)))
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
	   :opts (make-opts (:name file :description "path to skelfile")))
	  (:name build
	   :description "build artifacts"
	   :opts (make-opts (:name target :description "target artifact to build")))
	  (:name run)))

(defun run ()
  (with-cli (opts cmds) $cli
    (loop for o in (active-opts $cli t)
	  do (do-opt o))
    (debug! (cli-opts $cli) (cli-cmd-args $cli) (cli-cmds $cli))
    (cond
      ((member "build" *argv* :test #'string=) (nyi!))
      ((member "describe" *argv* :test #'string=) (describe (find-skelfile #P"." :load t)))
      ((member "-h" *argv* :test #'string=) (print-help $cli))
      ((member "-v" *argv* :test #'string=) (print-version $cli))
      ((member "-f" *argv* :test #'string=) (nyi!))
      (t (print-help $cli)))))
       
(defmain ()
  (run)
  (sb-ext:exit :code 0))
