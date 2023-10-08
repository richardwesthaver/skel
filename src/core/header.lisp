;;; File Headers
(deftype file-header-kind () '(member :source :shebang))

(declaim (inline %make-file-header))
(defstruct (file-header (:constructor %make-file-header)
			   (:conc-name sk-fh-))
  (kind :source :type file-header-kind)
  (str "" :type string))

(defun make-file-header (kind string)
  (%make-file-header :kind kind :str (or string "")))

(defun make-source-file-header (str)
  (make-file-header :source str))

(defun make-shebang-file-header (str)
  (make-file-header :shebang str))

;; TODO 2023-09-17: this should be a struct I think - file-header maybe?
(defun make-source-header-comment (name &key (cchar #\;) (timestamp nil) (description nil) (opts nil))
  "Generate a generic file-header with optional timestamp, description, and opts."
  (format nil "~A ~A~A~A~A~%" (make-string 3 :initial-element cchar) 
	  name
	  (if timestamp
	      (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time) 0)
		(format nil " @ ~4,'0d-~2,'0d-~2,'0d.~2,'0d:~2,'0d:~2,'0d" y mo d h m s))
	      "")
	  (if description
	      (format nil " --- ~A" description)
	      "")
	  (if opts
	      (format nil " -*- ~{~A~^;~} -*-" opts)
	      "")))

(defun make-shebang-comment (shell &rest args)
  "Generate a shebang file-header line."
  (format nil "#~A ~{~A~^ ~}~%" shell args))
