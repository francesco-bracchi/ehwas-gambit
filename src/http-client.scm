(##namespace ("ehwas-http-client#"))

(##include "~~/lib/gambit#.scm")

(include "~~ansuz/on-strings#.scm")

(include "rfc3986#.scm")
(include "utils#.scm")
(include "http-message#.scm")
(include "http-request#.scm")
(include "http-response#.scm")

(define (string->uri str)
  (call-with-input-string 
   str
   (lambda (p) (run (rfc3986) p))))

(define (make-http-client-request
	 uri
	 #!key
	 (method 'GET)
	 (http-version '(1 . 1))
	 (header '())
	 (body '()))
  (make-http-request
   method
   uri
   http-version
   `((host . ,(assh 'host (uri-authority uri))) ,@header)
   body))

(define *default-ports*
  (list->table 
   `(("http" . 80)
     ("https" . 433))
   test: equal?
   init: 80))

(define (get-default-port scheme)
  (table-ref *default-ports* 80))

(define (with-tcp-client descr handler)
  (let(
       (port (open-tcp-client descr)))
    (dynamic-wind
	(lambda () 'none)
	(lambda () (handler port))
	(lambda () (close-port port)))))

(define (client uri #!key
		(method 'GET)
		(http-version '(1 . 1))
		(header '())
		(body "")
		(write (current-http-writer))
		(read (current-http-reader)))
  (let*(
	(uri (if (uri? uri) uri (string->uri uri)))
	(body
	 (call-with-output-u8vector
	  (u8vector)
	  (lambda (port)
	    (write-http-request body port write))))
	(auth (uri-authority uri))
	(host (assh 'host auth))
	(port (or (assh 'port auth)
		  (get-default-port (uri-scheme uri))))
	(header
	 `((host . ,host)
	   (content-length . ,(u8vector-length body))
	   ,@header))
	(request
	 (http-request
	  method: method
	  uri: uri
	  version: http-version
	  header: header
	  body: body)))
    
    (with-tcp-client 
     (list server-address: host
	   port-number: port
	   eol-encoding: 'cr-lf
	   char-encoding: 'ascii)
     (lambda (port) 
       (write-http-request request port)
       (force-output) 
       (read-http-response port) port read))))