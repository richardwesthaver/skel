(defpackage :skel.tests
  (:use :cl :skel :macs.rt))
(in-package :skel.tests)
(defsuite skel.tests)
(in-suite skel.tests)
(deftest sanity (print (describe-skeleton *default-skelfile*)))
(deftest sanity1 (print (describe-skeleton *default-skelfile*)))
