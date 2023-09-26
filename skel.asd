#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "skel"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/skel/issues"
  :depends-on (:uiop :sb-posix :sb-bsd-sockets :macs :sxp :organ)
  :in-order-to ((test-op (test-op skel/tests)))
  :serial t
  :components ((:file "skel")
	       (:file "vc")
	       (:file "asdf")
	       (:file "make")
	       (:file "virt")))

(defsystem "skel/cli"
  :depends-on (:skel :macs)
  :version "0.1.0"
  :build-operation "program-op"
  :build-pathname "skel"
  :entry-point "skel.cli:main"
  :perform (test-op (test-op skel/tests))
  :components ((:file "cli")))

(defsystem "skel/tests"
  :depends-on (:skel :macs :macs/rt)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:macs.rt '#:do-tests)))

