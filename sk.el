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
		  (require 'fu (expand-file-name "lisp/fu" user-emacs-directory))
		  (require 'sxp (expand-file-name "sxp/sxp" user-dev-directory))
		  (defvar skel-debug nil)
		  (when skel-debug (require 'ede)))

(defconst skel-version "0.1.0")

(defgroup skel nil
  "skel customization group.")

(defcustom skel-keymap-prefix "C-c C-."
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

(define-minor-mode skel-mode
  "skel minor-mode."
  :global t
  :lighter " sk"
  :group 'skel
  :version skel-version)

(defvar skel-hashtable (make-hash-table :test #'equal)
  "Internal table of available skeletons.")

(defvar skel-stack nil "Internal stack of skeletons.")

(defcustom skel-state 'passive
  "State toggle for the `skel' system. Base states are 'passive' and
'active'."
  :type 'symbol
  :group 'skel)

(defvar skel-active-map nil
  "List of cons cells of the form '(SYM . BODY...)' where SYM is a member of `skel-triggers'.")

(defvar skel-passive-map nil
  "list of cons cells of the form '(SYM . BODY...)' where SYM is a member of `skel-triggers'.")

(defmacro make-id (&optional pre)
  `(let ((pre ,(if-let (pre) (concat skel-id-prefix "-" pre "-") (concat skel-id-prefix "-")))
	 (current-time-list nil))
     (symb pre (prog1 gensym-counter (setq gensym-counter (1+ gensym-counter))) (format "%x" (car (current-time))))))

(defmacro defcmd (name &rest body) `(defun ,name nil (interactive) ,@body))

(defclass sk (sxp)
  ((id :initarg :id :initform (make-id)))
  :documentation "Base class for skeleton objects. Inherits from `sxp'."
  :abstract t)

(defcmd sk-classes (eieio-class-children 'sk))

(defmacro def-sk-class (name doc &optional slots superclasses)
  "Define a new class with superclass of `skel'+SUPERCLASSES, SLOTS,
DOC, and NAME."
  (declare (indent 1))
  `(defclass ,(symb "sk-" name)
     ,(if superclasses `(sk ,@superclasses) '(sk))
     ,(if slots
	  `(,@slots
	    (:id :initarg :id :initform (make-id ,(symbol-name name)) :accessor id))
	`((:id :initarg :id :initform (make-id ,(symbol-name name)) :accessor id)))
     :documentation ,doc))

(def-sk-class project "Project skeleton class.")
(def-sk-class target "Target skeleton class.")
(def-sk-class source "Source skeleton class.")
(def-sk-class compiler "Compiler skeleton class.")
(def-sk-class action "Action skeleton class.")
(def-sk-class file "File skeleton class.")
(def-sk-class script "Script skeleton class.")
(def-sk-class document "Doc skeleton class.")
(def-sk-class config "Config skeleton class.")

(provide 'skel)
;;; skel.el ends here
