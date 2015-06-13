;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

;;; This file defines a set of generic functions which security systems should provide methods for.

(defpackage #:glass
  (:use #:cl)
  (:nicknames #:gss)
  (:export #:acquire-credentials
	   #:initialize-security-context
	   #:accept-security-context
	   #:get-mic
	   #:verify-mic
	   #:wrap
	   #:unwrap
	   #:context-principal-name
	   #:gss-error))

(in-package #:glass)

;; this should return a "credential handle" for input into initialize-security-context or accept-security-context
;; but somehow the "credentials" need to be input into this function.
(defgeneric acquire-credentials (mech-type principal &key)
  (:documentation "Acquire credentials for the principal named. Returns CREDENTIALS, for input into INITIALIZE-SECURITY-CONTEXT and ACCEPT-SECURITY-CONTEXT. 
c.f. GSS_Acquire_cred.

MECH-TYPE ::= symbol naming the authentication mechamism.

PRINCIPAL ::= the name of the principal you are requesting credentials for. NIL assumes default.

Returns an opaque credential object to be used in subsequent calls.
"))

;; NOTE: this function MUST return an InitialContextToken structure, i.e. an ASN.1 DER encoded structure. 
;; This is is defined in the specification.
;; Q: we have the necessary serializer in cerberus, should we provide it here instead? 
;; A: that is hard to do, because the necessary codes are rather tightly coupled (it's implemented by the 
;; cerberus DER-serializer). Moving it here would not be simple.

;; The context should either be an initial context returned from ACQUIRE-CREDENTIAL or a context that 
;; has been returned from a prior call to INITIALIZE-SECURITY-CONTEXT. In the later case, this implies
;; validating a reply from the application server (to verify its identity). The received token should also 
;; be passed in via a keyword parameter.
(defgeneric initialize-security-context (context-or-credentials &key)
  (:documentation "Returns a security context to be sent to the application server. c.f. GSS_Init_sec_context.

On the first call CONTEXT-OR-CREDENTIALS should be the result of the initial call to ACQUIRE-CREDENTIALS.

On subsequent calls, CONTEXT-OR-CREDENTIALS should be the context returned from the previous call to INITIALIZE-SECURITY-CONTEXT.

Returns (values context buffer continue-needed) where context is an opaque object to be used in subsequent calls to this or other functions. Buffer 
is either an opaque octet-vector, which should be sent to the server, or nil if the context has been completed. Continue needed is
a boolean indicating whether further calls to this function need to made before the authentication is complete.

May signal conditions of type GSS-ERROR."))

(defgeneric accept-security-context (context-or-credentials buffer &key)
  (:documentation "For the server to accept a security context from the client.

On the first call to this function, CONTEXT-OR-CREDENTIALS should be a credential object as returned from the initial
call to ACQUIRE-CREDENTIALS. Subsequent calls CONTEXT-OR-CREDENTIALS should be the context returned from the previous call
to this function.

BUFFER is the opaque octet vector sent from the client.

Returns (values context response-buffer continue-needed) where CONTEXT is the context to be used in subsequent calls to this function or other
glass functions. RESPONSE-BUFFER is either an opaque octet vector to be sent back to the client, or nil if the context has been completed.
CONTINUE-NEEDED is a boolean indicating whether further calls to this function are required before authentication has completed.

May signal GSS-ERROR if authentication fails.
"))

;; Get_MIC()
(defgeneric get-mic (context message &key)
  (:documentation "Compute a checksum over the message. C.f. GSS_GetMIC.

MESSAGE ::= octet array containing the plaintext.

Returns an octet array."))

;; GSS_VerifyMIC()
(defgeneric verify-mic (context message message-token &key)
  (:documentation "Verify the checksum. c.f. GSS_VerifyMIC

MESSAGE ::= octet array containing the original message that was checksum'ed.
MESSAGE-TOKEN ::= the checksum, i.e. result of calling GET-MIC.

Returns T if verified."))

;; ;; GSS_Wrap()
(defgeneric wrap (context message &key)
  (:documentation "Encrypt the message. c.f. GSS_Wrap
MESSAGE ::= octet array containing the plaintext message

Returns an octet array contining the encrypted message."))

;; ;; GSS_Unwrap()
(defgeneric unwrap (context-handle buffer &key)
  (:documentation "Decrypt the message. c.f. GSS_Unwrap

BUFFER ::= the wrapped message, as returned by WRAP. 

Returns the decrypted plaintext."))


;; something like GSS_Display_Name
(defgeneric context-principal-name (context &key)
  (:documentation "Returns a string which represents the name of the principal to which is authenticated by this context. 
This function should be used by servers wishing to get some information on the identity of the client.
"))
