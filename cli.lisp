;;; cli.lisp --- skel cli

;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel)
  (:shadowing-import-from :sb-ext :defglobal)
  (:export :main))

(in-package :skel.cli)

(defvar *skel-help* "usage: skel [global] <command> [<args>] -- <sxp>

global options:
  -q|--quiet    silence all output
  -l|--log      set log level (debug,info,trace,warn)

top-level options:
  -h|--help     pring help
  -v|--version  print version

commands:
  (build)
  (run)
  (test)
  (check)
  (deploy)
  (init)
  (new)
  (make)
  (compile)
  (tangle)
  (weave)
")

(defvar *opts* (make-opts help version log "file"))
(defvar *cmds* (make-cmds status build run))
(defparameter *cli*
  (make-cli t :name "skel"
	      :version "0.1.1"
	      :help *skel-help*
	      :opts (make-opts help version log "file")
	      :cmds (make-cmds status build run)))

(defun run ()
  (with-cli (opts cmds) *cli*
    (cond
      ((member "status" *argv* :test #'string=) (nyi!))
      ((member "-h" *argv* :test #'string=) (print-help *cli*))
      ((member "-v" *argv* :test #'string=) (print-version *cli*))
      (t (describe-project)))))
       
(defmain ()
  (run) (sb-ext:exit :code 0))
