;;; tests.lisp --- skel tests
(defpackage :skel.tests
  (:use :cl :skel :macs.rt))
(in-package :skel.tests)

(defsuite :skel)
(in-suite :skel)

(defun skels (c)
  (let ((s))
    (loop for i from 1 to c
	  do (push (sk-id (make-instance 'sk-project)) s))
    s))

(deftest sanity ()
  (is (eq t (apply #'/= (skels 10000)))))

(deftest header-comments ()
  "Make sure header comments are generated correctly. 

This covers variations of make-source-header-comment, make-source-file-header,
make-shebang-comment, and make-shebang-file-header."
  (make-shebang-file-header (make-shebang-comment "/dev/null"))
  (make-source-file-header (make-source-header-comment "foo-test"
						       :timestamp t
						       :description "nothing to see here"
						       :opts '("Definitely-Not_Emacs: T;"))))
