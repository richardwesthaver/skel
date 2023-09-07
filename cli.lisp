(defpackage skel.cli
  (:use :cl :skel :macs.cli)
  (:export :main))
(in-package :skel.cli)

(defvar *skel-banner* "SKEL --- THE PROJECT COMPILER")
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
█████████████████████████▄▄▄▄▄▄▄█████████████████████████")
(defvar *skel-help* "
usage: skel [global] <command> [<args>] -- <sxp>

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
	    :opts #((make-cli :opt :name "help")
		    (make-cli :opt :name "version")
		    (make-cli :opt :name "quiet")
		    (make-cli :opt :name "log"))
	    :cmds #((make-cli :cmd :name "show"))))

(defun run ()
  (with-cli (opts cmds help version) *cli*
    (format t "~A~A~A" *skel-banner* *skel-art* *skel-help*)))

;; (print-api)
(defmain () (run))
