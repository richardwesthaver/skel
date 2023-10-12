;;; cli.lisp --- skel cli

;; $args, $argv $cli $opt
;;; Code:
(defpackage skel.cli
  (:use :cl :cond :cli :skel :fmt :log :fu :readtables)
  (:import-from :sb-posix :getcwd)
  (:import-from :uiop :println)
  (:export :main))

(in-package :skel.cli)

(defun skc-build ())

(defvar skc-file-prompt-history '(""))
(make-prompt! skc-file "file")
(defvar skc-name-prompt-history '(""))
(make-prompt! skc-name "name")

(defcmd skc-init
    (let ((file (when $args (pop $args)))
	  (name (if (> $argc 1) (pop $args))))
      (handler-bind
	  ((sb-ext:file-exists 
	     #'(lambda (s)
		 (println (format nil "file already exists: ~A" (or file *default-skelfile*)))
		 (let ((f2 (skc-file-prompt)))
		   (if (string= f2 "") (error s)
		       (use-value f2 s))))))
	(init-skelfile file name))))

(defcmd skc-describe
    (describe 
     (find-skelfile 
      (if $args (pathname (car $args))
	  #P".")
      :load t)))

(defcmd skc-inspect
    (inspect
     (find-skelfile
      (if $args (pathname (car $args))
	  (cli-cwd $cli))
      :load t)))

(defcmd skc-show
    (find-skelfile
     (if $args (pathname (car $args))
	 (cli-cwd $cli))
     :load t)
  (terpri))

(define-cli $cli
  :name "skel"
  :version "0.1.1"
  :description "A hacker's project compiler and build tool."
  :thunk skc-show
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
	  (:name init
	   :description "initialize a skelfile in the current directory"
	   :opts (make-opts 
		   (:name name :description "project name" :kind string))
	   :thunk skc-init)
	  (:name show
	   :description "describe the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile" :kind file))
	   :thunk skc-describe)
	  (:name inspect
	   :description "inspect the project skelfile"
	   :opts (make-opts (:name file :description "path to skelfile" :kind file))
	   :thunk skc-inspect)
	  (:name build
	   :description "build project targets"
	   :opts (make-opts (:name target :description "target to build" :kind string))
	   :thunk skc-build)
	  (:name run
	   :description "run a script or command")
	  (:name push
	   :description "push the current project upstream")
	  (:name pull
	   :description "pull the current project from remote")
	  (:name clone
	   :description "clone a remote project")
	  (:name commit
	   :description "commit changes to the project vc")
	  (:name edit
	   :description "edit a project file")))

(defun run ()
  (in-readtable *macs-readtable*) ;; should be in sxp
  ;; KLUDGE 2023-10-11: overwritten when --cfg is used
  (unless (boundp '*skel-user-config*)
    (init-skel-user-config))
  (with-cli () $cli
    (do-cmd $cli)
    (debug-opts $cli)
    (debug! "loaded skelrc:" *skel-user-config*)))

(defmain ()
  (run)
  (sb-ext:exit :code 0))
