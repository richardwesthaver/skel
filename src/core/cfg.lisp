(in-package :skel)

;;; Vars
(declaim (type vc-designator *default-skel-vc-kind*))
(deftype vc-designator () '(member :hg :git nil))
(defparameter *default-skel-vc-kind* :hg)

(declaim (type sk-project *skel-project*))
(defvar *skel-project*)

;; TODO (defparameter *skel-project-registry* nil)
;; TODO (defvar *skelfile-boundary* nil "Set an upper bounds on how
;; many times and how far to walk an arbitrary file directory.")

(declaim (type string *default-skel-user* *default-skelfile* *skelfile-extension*))
(defparameter *default-skel-user* (uid-username (getuid)))
(defparameter *default-skelfile* "skelfile")
(defparameter *skelfile-extension* "sk")

(declaim (type pathname *default-skel-cache* *default-user-skel-config* *default-global-skel-config*))
(defparameter *default-skel-cache* (make-pathname :directory (format nil "/home/~a/.cache/skel" *default-skel-user*)))
(defparameter *default-user-skel-config* (make-pathname :name (format nil "/home/~a/.skelrc" *default-skel-user*)))
(defparameter *default-global-skel-config* (make-pathname :name "/etc/skelrc"))
