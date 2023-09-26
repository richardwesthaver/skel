;;; tests.lisp --- skel tests
(defpackage :skel.tests
  (:use :cl :skel :skel.make :skel.asdf :skel.vc :rt)
  (:import-from :uiop :file-exists-p))
(in-package :skel.tests)

(defsuite :skel)
(in-suite :skel)

(defun gen-tmp-path (ext)
  (format nil "/tmp/~A.~A" (gensym) ext))

(defvar *tmp-path* "./")
(defvar %tmp)

;; doesn't really need to be a macro but w/e
(defmacro with-tmp-file (ext &body body)
  `(progn
     (setq %tmp (format nil "~A~A.~A" *tmp-path* (gensym) ,ext))
     ,@body
     (is (file-exists-p %tmp))
     (ignore-errors (delete-file %tmp))))

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
  (with-tmp-file "sk"
    (is (init-skelfile %tmp)))
  (with-tmp-file "sk"
    (is (sk-make-file (make-instance 'sk-project :name "nada") :path %tmp))))

(deftest makefile ()
  "Make sure makefiles are making out ok."
  (with-tmp-file "mk"
    (let ((mk (make-instance 'makefile :name "foobar")))
      (sk-make-file mk :path %tmp))))

(deftest vm ()
  "EXPERIMENTAL"
  (is (let ((vm (make-sk-vm 201)))
	(dotimes (i 200)
	  (sks-pop vm))
	t))
  (let ((vm (make-sk-vm 1)))
    (is (sks-pop vm))
    (signals simple-error (sks-pop vm))))

