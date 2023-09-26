;;; tests.lisp --- skel tests
(defpackage :skel.tests
  (:use :cl :skel :skel.make :skel.asdf :skel.vc :rt)
  (:import-from :uiop :file-exists-p))

(in-package :skel.tests)

(defsuite :skel)
(in-suite :skel)

(defvar %tmp)
(defun gen-tmp-path (ext)
  (setq %tmp (format nil "/tmp/~A.~A" (gensym) ext)))

(defun with-tmp-file (file &rest body)
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
  (with-tmp-file (gen-tmp-path "sk")
    (is (sk-write-file (make-instance 'sk-project :name "nada" :path %tmp) :if-exists :overwrite))
    (setq %tmp (gen-tmp-path "sk"))
    (is (init-skelfile %tmp))
    (is (print (sk-read-file (make-instance 'sk-project) :path %tmp)))))

(deftest makefile ()
  "Make sure makefiles are making out ok."
    (with-tmp-file (gen-tmp-path "mk")
      (let ((mk (make-instance 'makefile :name "foobar" :path %tmp)))
	(is (null (sk-write-file mk :if-exists :overwrite))))))

(deftest vm ()
  "EXPERIMENTAL"
  (is (let ((vm (make-sk-vm 201)))
	(dotimes (i 200)
	  (sks-pop vm))
	t))
  (let ((vm (make-sk-vm 1)))
    (is (sks-pop vm))
    (signals simple-error (sks-pop vm))))

