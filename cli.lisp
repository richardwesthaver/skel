;;; cli.lisp --- skel cli

;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel :fmt)
  (:import-from :sb-posix :getcwd)
  (:export :main))

(in-package :skel.cli)

(defvar *opts* (make-opts 
		'(:name help :description "print this message")
		'(:name version :description "print version")
		'(:name log :global t :description "set log level (debug,info,trace,warn)")
		'(:name input :description "input source")
		'(:name output :description "output target")))

(defvar *cmds* (make-cmds
		'(:name status
		  :description "print the status of the current project")
		`(:name describe
		  :description "describe the project skelfile"
		  :opts ,(make-opts '(:name file :description "path to skelfile")))
		`(:name build
		  :description "build artifacts"
		  :opts ,(make-opts '(:name target :description "target artifact to build")))
		'(:name run)))

(defvar *cli*
  (make-cli t :name "skel"
	      :version "0.1.1"
	      :description "the hacker's project management tool."
	      :opts *opts*
	      :cmds *cmds*))

(defun run ()
  (with-cli (opts cmds) *cli*
    (cond
      ((member "build" *argv* :test #'string=) (nyi!))
      ((member "describe" *argv* :test #'string=) (describe (find-skelfile #P"." :load t)))
      ((member "-h" *argv* :test #'string=) (print-help *cli*))
      ((member "-v" *argv* :test #'string=) (print-version *cli*))
      ((member "-f" *argv* :test #'string=) (nyi!))
      (t (print-help *cli*)))))
       
(defmain ()
  (run)
  (sb-ext:exit :code 0))
