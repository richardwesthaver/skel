;;; skel.lisp --- skeleton library

;; A hacker's project compiler.

;;; Commentary:

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute) (require 'sb-posix))

(defpackage skel
  (:use :cl :sxp :cond :fu :fmt :sb-mop :log)
  (:import-from :sb-posix :getcwd :getuid)
  (:import-from :sb-unix :uid-username)
  (:shadowing-import-from :uiop :pathname-parent-directory-pathname :read-file-forms)
  (:export
   :*skel-project* :*skel-project-registry* :*default-skelfile* :*default-skel-user* 
   :*default-skel-cache* :*default-user-skel-config* :*default-global-skel-config* :*skelfile-extension*
   :make-file-header :make-shebang-file-header :make-source-file-header :file-header-kind
   :make-source-header-comment :make-shebang-comment :*skelfile-boundary* :find-skelfile :load-skelfile
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe :make-sk-rule 
   :sk-description
   :sk-kind :sk-rules :sk-id :sk-version :sk-name :sk-documents :sk-document :sk-command
   :sk-compile
   :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :describe-skeleton :describe-project :init-skelfile
   :sk-write :sk-write-string :sk-writeln :sk-write-file :sk-read-file
   :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push))

(in-package :skel)
