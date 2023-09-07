(defpackage skel.cli
  (:use :cl :skel :macs.cli)
  (:export :main))
(in-package :skel.cli)

(defvar *skel-art* "
█████████████████████████████████████████████████████████
██████████████████████▀▀▀░░░░░░░▀▀▀██████████████████████
███████████████████▀░░░░░░░░░░░░░░░░░▀███████████████████
██████████████████│░░░░░░░░░░░░░░░░░░░│██████████████████
█████████████████▌│░░░░░░░░░░░░░░░░░░░│▐█████████████████
█████████████████░└┐░░░░░░░░░░░░░░░░░┌┘░█████████████████
█████████████████░░└┐░░░░░░░░░░░░░░░┌┘░░█████████████████
█████████████████░░┌┘▄▄▄▄▄░░░░░▄▄▄▄▄└┐░░█████████████████
█████████████████▌░│██████▌░░░▐██████│░▐█████████████████
██████████████████░│▐███▀▀░░▄░░▀▀███▌│░██████████████████
█████████████████▀─┘░░░░░░░▐█▌░░░░░░░└─▀█████████████████
█████████████████▄░░░▄▄▄▓░░▀█▀░░▓▄▄▄░░░▄█████████████████
███████████████████▄─┘██▌░░░░░░░▐██└─▄███████████████████
████████████████████░░▐█─┬┬┬┬┬┬┬─█▌░░████████████████████
███████████████████▌░░░▀┬┼┼┼┼┼┼┼┬▀░░░▐███████████████████
████████████████████▄░░░└┴┴┴┴┴┴┴┘░░░▄████████████████████
██████████████████████▄░░░░░░░░░░░▄██████████████████████
█████████████████████████▄▄▄▄▄▄▄█████████████████████████
")
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

(defvar *cli*
  (make-cli t
	    :name "skel"
	    :version "0.1.1"
	    :banner *skel-art*
	    :help *skel-help*
	    :opts #((make-cli :opt :name "help")
		    (make-cli :opt :name "version")
		    (make-cli :opt :name "quiet")
		    (make-cli :opt :name "log"))
	    :cmds #((make-cli :cmd :name "show"))))

(defun run ()
  (with-cli (opts cmds) *cli* (cli-args)
    (cond
      ((member "show" *argv* :test #'string=) (describe-project))
      ((member "-h" *argv* :test #'string=) (print-help *cli*))
      ((member "-v" *argv* :test #'string=) (print-version *cli*))
      (t (print-banner *cli*)))))
       

;; (print-api)
(defmain () (run) (sb-ext:exit :code 0))
