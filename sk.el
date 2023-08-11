;;; skel.el --- skel Emacs API -*- lexical-binding: t; -*-

;; Copyright (C) 2023  anticorp

;; Author: ellis <ellis@rwest.io>
;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(eval-and-compile (require 'eieio)
		  (require 'cl-lib)
		  (require 'sxp (path-join user-dev-directory "sxp/sxp"))
		  (defvar skel-debug nil)
		  (when skel-debug (require 'ede)))

(defconst skel-version "0.1.0")

(defgroup skel nil
  "skel customization group.")

(defcustom skel-keymap-prefix "C-c c x"
  "Prefix for `skel-mode' keymap."
  :type 'string
  :group 'skel)

(defcustom skel-triggers nil
  "Association of symbols to a specific condition which can be used
to trigger `skel-actions' based on the `skel-behavior' value."
  :type 'cons
  :group 'skel)

(defcustom skel-actions nil
  "Array of 'actions' which may be performed on skeletons."
  :type 'obarray
  :group 'skel)

(defcustom skel-id-prefix "sk"
  "Default prefix for `make-id'."
  :type 'string
  :group 'skel)

(defvar skel-hashtable (make-hash-table :test #'equal)
  "Internal table of available skeletons.")
(defvar skel-stack nil "Internal stack of skeletons.")

(defvar skel-active-map nil
  "List of cons cells of the form '(SYM . BODY...)' where SYM is a member of `skel-triggers'.")
(defvar skel-passive-map nil
  "list of cons cells of the form '(SYM . BODY...)' where SYM is a member of `skel-triggers'.")

(defmacro make-id (&optional pre)
  `(let ((pre ,(if-let (pre) (concat skel-id-prefix "-" pre "-") (concat skel-id-prefix "-")))
	 (current-time-list nil))
     (symb pre (prog1 gensym-counter (setq gensym-counter (1+ gensym-counter))) (format "%x" (car (current-time))))))

(defclass sk (sxp)
  ((:id :initarg :id :initform (make-id)))
  :documentation "Base class for skeleton objects. Inherits from `sxp'."
  :abstract t)

(defmacro def-skel-class (name doc &optional slots superclasses)
  "Define a new class with superclass of `skel'+SUPERCLASSES, SLOTS,
DOC, and NAME. PFX is used as the first argument to `make-id' in
the 'initform' key of the ':id' slot."
  (declare (indent 1))
  `(defclass ,(symb "sk-" name)
     ,(if superclasses `(skel ,@superclasses) '(skel))
     ,(if slots
	  `(,@slots
	    (:id :initarg :id :initform (make-id ,(symbol-name name)) :accessor id))
	`((:id :initarg :id :initform (make-id ,(symbol-name name)) :accessor id)))
     :documentation ,doc))

(def-skel-class project "Project skeleton class.")
(def-skel-class target "Target skeleton class.")
(def-skel-class source "Source skeleton class.")
(def-skel-class compiler "Compiler skeleton class.")
(def-skel-class action "Action skeleton class.")
(def-skel-class file "File skeleton class.")
(def-skel-class script "Script skeleton class.")
(def-skel-class doc "Doc skeleton class.")
(def-skel-class config "Config skeleton class.")

(define-minor-mode skel-mode
  "skel minor-mode."
  :global t
  :lighter " sk"
  :group 'skel
  :version skel-version)

(provide 'skel)
;;; skel.el ends here
