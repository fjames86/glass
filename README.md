# glass
Common Lisp GSS layer

## 1. Introduction
The GSSAPI specifies a generalized mechanism for defining security service APIs. 

## 2. Usage
This package provides a set of generic functions. Systems which provide security systems should provide 
methods for these generics.

### 2.1 Kerberos
Currently the only security system implementing these functions is [cerberus](https://github.com/fjames86/cerberus),
which provides Kerberos v5 support.

```
CL-USER> (defvar *context* 
                 (gss:acquire-credential :kerberos 
                                         :principal (cerberus:principal "host" :instance "host.name.com" :type :srv-host) 
                                         :kdc-address "10.1.1.1" 
                                         :username "username" :password "1234" :realm "REALM"))
*CONTEXT*
CL-USER> (defvar *buffer* (gss:initialize-security-context *context*))
*BUFFER*
;; send the buffer to the application server

CL-USER> (defvar *context2* 
                 (gss:acquire-credential :kerberos :username "Administrator" :password "1234" :realm "REALM"))
*CONTEXT2*
CL-USER> (gss:accept-security-context *context2* *buffer*)

;; compute checksums
CL-USER> (gss:get-mic *context* #(1 2 3 4))
CL-USER> (gss:verify-mic *context2* (gss:get-mic *context* #(1 2 3 4)))

;; encrypt message
CL-USER> (gss:wrap *context* #(1 2 3 4))
CL-USER> (gss:unwrap *context2* #(1 2 3 4))

```

## 3. License
Licensed under the terms of the MIT license.

Frank James 
May 2015.

