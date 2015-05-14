;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(in-package #:glass)

(defparameter *gss-status-codes*
  '((:BAD-BINDINGS            "channel binding mismatch")
    (:BAD-MECH                "unsupported mechanism requested")
    (:BAD-NAME                "invalid name provided")
    (:BAD-NAMETYPE            "name of unsupported type provided")
    (:BAD-STATUS              "invalid input status selector")
    (:BAD-SIG                 "token had invalid integrity check")
    (:BAD-MIC                 "preferred alias for GSS_S_BAD_SIG")
    (:CONTEXT-EXPIRED         "specified security context expired")
    (:CREDENTIALS-EXPIRED     "expired credentials detected")
    (:DEFECTIVE-CREDENTIAL    "defective credential detected")
    (:DEFECTIVE-TOKEN         "defective token detected")
    (:FAILURE                 "failure, unspecified at GSS-API level")
    (:NO-CONTEXT              "no valid security context specified")
    (:NO-CRED                 "no valid credentials provided")
    (:BAD-QOP                 "unsupported QOP value")
    (:UNAUTHORIZED            "operation unauthorized")
    (:UNAVAILABLE             "operation unavailable")
    (:DUPLICATE-ELEMENT       "duplicate credential element requested")
    (:NAME-NOT-MN             "name contains multi-mechanism elements")
    ;; informational statuses
    (:COMPLETE                "normal completion")
    (:CONTINUE-NEEDED         "continuation call to routine required")
    (:DUPLICATE-TOKEN         "duplicate per-message token detected")
    (:OLD-TOKEN               "timed-out per-message token detected")
    (:UNSEQ-TOKEN             "reordered (early) per-message token detected")
    (:GAP-TOKEN               "skipped predecessor token(s) detected")))

(define-condition gss-error (error)
  ((major :initarg :major :initform nil :reader gss-error-major)
   (minor :initarg :minor :initform nil :reader gss-error-minor))
  (:report (lambda (c stream)
	     (let ((major (gss-error-major c))
		   (minor (gss-error-minor c)))
	       (format stream "GSS-ERROR")
	       (when major (format stream " ~A: ~A" major (cadr (assoc major *gss-status-codes*))))
	       (when minor (format stream " (~A)" minor))))))

