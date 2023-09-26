;;; tests.lisp --- skel tests
(defpackage :skel.tests
  (:use :cl :skel :rt))
(in-package :skel.tests)

(defsuite :skel)
(in-suite :skel)

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

(deftest skelfiles ()
  "Ensure skelfiles are created and loaded correctly and that they signal
the appropriate restarts."
  (let ((file (format nil "/tmp/~A.sk" (gensym))))
    (is (init-skelfile file))
    (is (delete-file file))))

(deftest vm ()
  "EXPERIMENTAL"
  (is (let ((vm (make-sk-vm 201)))
	(dotimes (i 200)
	  (sks-pop vm))
	t))
  (let ((vm (make-sk-vm 1)))
    (is (sks-pop vm))
    (signals simple-error (sks-pop vm))))

