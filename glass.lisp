;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


(defpackage #:glass
  (:use #:cl)
  (:nicknames #:gss)
  (:export #:acquire-credential
	   #:initialize-security-context
	   #:accept-security-context
	   #:get-mic
	   #:verify-mic
	   #:wrap
	   #:unwrap))

(in-package #:glass)

(defgeneric acquire-credential (mech-type principal &key)
  (:documentation "Acquire credentials for the principal named. Returns CREDENTIALS, for input into INITIALIZE-SECURITY-CONTEXT. c.f. GSS_Acquire_cred.

MECH-TYPE ::= symbol naming the authentication mechamism
PRINCIPAL ::= the security principal you are requesting credentials for.
"))

(defgeneric initialize-security-context (context &key)
  (:documentation "Returns a security context to be sent to the application server. c.f. GSS_Init_sec_context"))

(defgeneric accept-security-context (context buffer &key)
  (:documentation "CREDENTIALS are credentials for the server principal. CONTEXT is the packed 
buffer sent from the client. It should be as returned from INITIALIZE-SECURITY-CONTEXT.

c.f. GSS_Accept_sec_context
"))

;; Get_MIC()
(defgeneric get-mic (context message &key))

;; GSS_VerifyMIC()
(defgeneric verify-mic (context message message-token &key))

;; ;; GSS_Wrap()
(defgeneric wrap (context message &key))


;; ;; GSS_Unwrap()
(defgeneric unwrap (context-handle buffer &key))


