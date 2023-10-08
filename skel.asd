#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "skel"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/skel/issues"
  :depends-on (:uiop :sb-posix :sb-bsd-sockets :macs :sxp :organ)
  :in-order-to ((test-op (test-op skel/tests)))
  :components ((:file "src/pkg")
	       (:module "core"
		:pathname "src/core"
		:depends-on ("src/pkg")
		:components
		((:file "err")
		 (:file "util" :depends-on ("err"))
		 (:file "cfg" :depends-on ("util"))
		 (:file "proto" :depends-on ("util"))
		 (:file "obj" :depends-on ("proto"))
		 (:file "header" :depends-on ("util"))
		 (:file "vc" :depends-on ("obj" "header"))
		 (:file "virt" :depends-on ("obj" "header"))
		 (:file "mk" :depends-on ("vc"))
		 (:file "vm" :depends-on ("obj" "virt" "mk"))))
	       (:module "comp" ;; compilers
		:pathname "src/comp"
		:depends-on ("core")
		:components
		((:file "asd")
		 (:file "containerfile")
		 (:file "ignore")
		 (:file "makefile")))
	       (:module "tools" ;; tools
		:pathname "src/tools"
		:depends-on ("core")
		:components
		((:file "deploy")
		 (:file "viz")))
	       (:module "ext" ;; extensions
		:pathname "src/ext"
		:depends-on ("core" "comp")
		:components
		((:file "asdf")))))

(defsystem "skel/cli"
  :depends-on (:skel :macs)
  :version "0.1.0"
  :build-operation "program-op"
  :build-pathname "skel"
  :entry-point "skel.cli:main"
  :perform (test-op (test-op skel/tests))
  :components ((:file "cli")))

(defsystem "skel/tests"
  :depends-on (:skel :macs :macs/rt :sxp)
  :components ((:file "tests"))
  :perform (test-op (op c) (uiop:symbol-call '#:macs.rt '#:do-tests)))

