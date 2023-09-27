;;; tests.lisp --- skel tests
(defpackage :skel.tests
  (:use :cl :skel :skel.make :skel.asdf :skel.vc :rt :sxp)
  (:import-from :uiop :file-exists-p))

(in-package :skel.tests)

(defsuite :skel)
(in-suite :skel)

(defvar %tmp)
(defun tmp-path (ext)
  (setq %tmp (format nil "/tmp/~A.~A" (gensym) ext)))

(defun do-tmp-path (file &rest body)
  (prog1 body
    (when (file-exists-p file) (delete-file file))))

(defun skels (c)
  (let ((s))
    (loop for i from 1 to c
	  do (push (sk-id (make-instance 'sk-project :name (gensym))) s))
    s))

(deftest sanity ()
  "IDs should be reasonably unique."
  (is (eq t (apply #'/= (skels 1000)))))

(deftest header-comments ()
  "Make sure header comments are generated correctly. 

This covers variations of make-source-header-comment, make-source-file-header,
make-shebang-comment, and make-shebang-file-header."
  (is (eq (type-of (make-shebang-file-header 
		    (make-shebang-comment "/dev/null"))) 
	  'skel::file-header))
  (is (eq (type-of (make-source-file-header 
		    (make-source-header-comment 
		     "foo-test"
		     :timestamp t
		     :description "nothing to see here"
		     :opts '("Definitely-Not_Emacs: T;"))))
	  'skel::file-header)))

(deftest skelfile ()
  "Ensure skelfiles are created and loaded correctly and that they signal
the appropriate restarts."
  (do-tmp-path (tmp-path "sk")
    (is (sk-write-file (make-instance 'sk-project :name "nada" :path %tmp) :if-exists :overwrite))
    (setq %tmp (tmp-path "sk"))
    (is (init-skelfile %tmp))
    (is (load-skelfile %tmp))
    (is (build-ast (sk-read-file (make-instance 'sk-project) :path %tmp)))))

(deftest makefile ()
  "Make sure makefiles are making out ok."
    (do-tmp-path (tmp-path "mk")
      (let* ((mk (make-instance 'makefile :name "foobar" :path %tmp :description "barfood"))
	     (tar (make-instance 'sk-target :path "foo.lisp"))
	     (src (make-instance 'sk-source :path "bar.c"))
	     (cmd (make-instance 'sk-command :body "echo 'muahaha'"))
	     (rule (make-sk-rule tar src "echo 'yolo'")))
	#-nil
	(is (null (sk-write-file mk :if-exists :supersede :path *default-makefile*)))
	(is (push-rule rule mk))
	(is (push-rule rule mk))
	(is (push-rule rule mk t))
	(is (push-rule rule mk t))
	(is (push-directive cmd mk))
	(is (push-directive cmd mk))
	(is (push-var '(a b) mk))
	(is (push-var '(b c) mk))
	(is (null (sk-write-file mk :if-exists :supersede))))))

(deftest vm ()
  "EXPERIMENTAL"
  (is (let ((vm (make-sk-vm 201)))
	(dotimes (i 200)
	  (sks-pop vm))
	t))
  (let ((vm (make-sk-vm 1)))
    (is (sks-pop vm))
    (signals simple-error (sks-pop vm))))
