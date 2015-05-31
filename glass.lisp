;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.


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
  (:documentation "Acquire credentials for the principal named. Returns CREDENTIALS, for input into INITIALIZE-SECURITY-CONTEXT. c.f. GSS_Acquire_cred.

MECH-TYPE ::= symbol naming the authentication mechamism
PRINCIPAL ::= the security principal you are requesting credentials for. NIL assumes default.
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
(defgeneric initialize-security-context (credentials &key)
  (:documentation "Returns a security context to be sent to the application server. c.f. GSS_Init_sec_context"))

(defgeneric accept-security-context (credentials buffer &key)
  (:documentation "CREDENTIALS are credentials for the server principal. CONTEXT is the packed 
buffer sent from the client. It should be as returned from INITIALIZE-SECURITY-CONTEXT.

c.f. GSS_Accept_sec_context
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
  (:documentation "Returns a string which represents the name of the principal to which is authenticated by this context."))
