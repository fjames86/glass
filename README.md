# glass
General Lisp Authentication Security Services (glass) is a Common Lisp GSS-compatible API. It provides a set of 
generic functions which systems providing authentication services should specialize. Users wishing to consume
these services should use these rather than functions exported directly from the providing packages. 

Currently the only system to provide these methods is [cerberus](https://github.com/fjames86/cerberus), which provides 
a Kerberos v5 implementation.

## 1. Introduction
The GSSAPI specifies a generalized mechanism for defining security service APIs. It is the most common way 
to consume Kerberos authentication. 

## 2. Usage
This package provides a set of generic functions. Systems which provide security systems should provide 
methods for these generics.

### 2.1 Kerberos
Currently the only security system implementing these functions is [cerberus](https://github.com/fjames86/cerberus),
which provides Kerberos v5 support.

```
;; client
CL-USER> (cerberus:logon-user "username@realm" "password" :kdc-address "10.1.1.1")
CL-USER> (defvar *credentials* 
                 (gss:acquire-credentials :kerberos 
                                         "host/host.name.com@realm"))
*CREDENTIALS*
CL-USER> (multiple-value-bind (context buffer) (gss:initialize-security-context *context* :mutual t)
           (defvar *client-context* context)
           (defvar *buffer* buffer))

;; send the buffer to the application server
CL-USER> (cerberus:logon-service "host/host.name.com@realm" "password")
CL-USER> (defvar *server-credentials* (gss:acquire-credentials :kerberos nil))
*SERVER-CREDENTIALS*
CL-USER> (multiple-value-bind (context response-buffer) (gss:accept-security-context *server-credentials* *buffer*)
            (defvar *server-context* context)
            (defvar *response-buffer* response-buffer))

;; send the response buffer back to the client and pass to INITIALIZE-SECURITY-CONTEXT so the 
;; client can authenticate the server
CL-USER> (gss:initialize-security-context *client-context* :buffer *response-buffer*)

;; compute checksums
CL-USER> (gss:get-mic *client-context* #(1 2 3 4))
CL-USER> (gss:verify-mic *server-context* (gss:get-mic *client-context* #(1 2 3 4)))

;; encrypt message
CL-USER> (gss:wrap *client-context* #(1 2 3 4))
CL-USER> (gss:unwrap *server-context* (gss:wrap *client-context* #(1 2 3 4)))

```

## 3. License
Licensed under the terms of the MIT license.

Frank James 
May 2015.

