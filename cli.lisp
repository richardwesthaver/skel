;;; cli.lisp --- skel cli

;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel :fmt)
  (:shadowing-import-from :sb-ext :defglobal)
  (:export :main))

(in-package :skel.cli)

(defvar *opts* (make-opts 
		'(:name help :description "print this message")
		'(:name version :description "print version")
		'(:name log :global t :description "set log level (debug,info,trace,warn)")
		'(:name input :description "input source")
		'(:name output :description "output target")))

(defvar *cmds* (make-cmds
		'(:name status :description "print the status of the current project")
		`(:name build :opts ,(make-opts '(:name target :description "target to build")))
		'(:name run)))

(defvar *cli*
  (make-cli t :name "skel"
	      :version "0.1.1"
	      :description "nyaaaaa"
	      :opts *opts*
	      :cmds *cmds*))

(defun run ()
  (with-cli (opts cmds) *cli*
    (cond
      ((member "status" *argv* :test #'string=) (nyi!))
      ((member "-h" *argv* :test #'string=) (print-help *cli*))
      ((member "-v" *argv* :test #'string=) (print-version *cli*))
      ((member "-f" *argv* :test #'string=) (nyi!))
      (t (describe-project)))))
       
(defmain ()
  (run) (sb-ext:exit :code 0))
