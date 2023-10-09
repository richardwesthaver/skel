;;; skel.lisp --- skeleton library

;; A hacker's project compiler.

;;; Commentary:

;;; Code:
(eval-when (:compile-toplevel :load-toplevel :execute) (require 'sb-posix))

(defpackage skel
  (:use :cl :sxp :cond :fu :fmt :sb-mop :log :list)
  (:import-from :sb-posix :getcwd :getuid)
  (:import-from :sb-unix :uid-username)
  (:shadowing-import-from :uiop :pathname-parent-directory-pathname :read-file-forms)
  (:export
   ;; cfg
   :*skel-project* :*skel-user-config* :*default-skelrc* :*skel-project-registry* 
   :*default-skelfile* :*default-skel-user* :*default-skel-cache* :*default-user-skel-config* 
   :*default-system-skel-config* :*skelfile-extension* :*skelfile-boundary*
   ;; proto
   :sk-run
   :sk-new
   :sk-save
   :sk-tangle
   :sk-weave
   :sk-call
   :sk-print
   :sk-load
   :sk-compile
   :rehash-object
   :sk-transform
   :sk-read-file
   :sk-write
   :sk-writeln
   :sk-write-string
   :sk-write-file
   :sk-read-file
   :sk-install-user-config
   ;; obj
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe :make-sk-rule 
   :sk-description :sk-kind :sk-rules :sk-id :sk-version :sk-name :sk-documents :sk-document 
   :sk-command :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   ;; header
   :make-file-header :make-shebang-file-header :make-source-file-header :file-header-kind
   :make-source-header-comment :make-shebang-comment 
   ;; utils
   :init-skelfile :init-skelrc :load-skelrc :find-skelfile :load-skelfile
   :describe-skeleton :describe-project 
   ;; vm
   :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push))

(in-package :skel)
