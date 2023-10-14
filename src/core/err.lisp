;;; Conditions
(in-package :skel)

(define-condition skel-syntax-error (sxp-syntax-error) ())

(define-condition skel-fmt-error (sxp-fmt-error) ())
