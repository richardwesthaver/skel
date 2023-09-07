;;; tests.lisp --- skel tests
(defpackage :skel.tests
  (:use :cl :skel :macs.rt))
(in-package :skel.tests)

(defsuite skel.tests)
(in-suite skel.tests)

(deftest sanity (:persist t))


