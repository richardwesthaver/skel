;;; tests.lisp --- skel tests
(defpackage :skel.tests
  (:use :cl :skel :macs.rt))
(in-package :skel.tests)

(defsuite skel.tests)
(in-suite skel.tests)

(deftest sanity ())

(deftest header-comments ()
  "Make sure header comments are generated correctly. 

This covers variations of make-source-header-comment, make-source-file-header,
make-shebang-comment, and make-shebang-file-header."
  (make-shebang-file-header (make-shebang-comment "/dev/null"))
  (make-source-file-header (make-source-header-comment "foo-test"
						       :timestamp t
						       :description "nothing to see here"
						       :opts '("Definitely-Not_Emacs: T;"))))
