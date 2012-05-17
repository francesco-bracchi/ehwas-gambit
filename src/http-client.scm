(##namespace ("ehwas-http-client#"))

(##include "~~/lib/gambit#.scm")

(include "~~ansuz/on-ports#.scm")

(include "rfc3986#.scm")
(include "utils#.scm")
(include "http-message#.scm")
(include "http-request#.scm")
(include "http-response#.scm")

(define (string->uri str)
  (call-with-input-string 
   str (lambda (p) 
	 (run (rfc3986) p))))

(define (make-http-client-request
	 uri
	 method
	 http-version 
	 header
	 body)
  (make-http-request
   http-version
   header 
   body
   method
   uri))

(define *default-ports*
  (list->table 
   `(("http" . 80)
     ("https" . 433))
   test: equal?
   init: 80))

(define (get-default-port scheme)
  (table-ref *default-ports* 80))

(define (with-tcp-client descr handler)
  (let((port (open-tcp-client descr)))
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
  (let*((uri (if (uri? uri) uri (string->uri uri)))
	(auth (uri-authority uri))
	(host (assh 'host auth))
	(port (or (assh 'port auth)
		  (get-default-port (uri-scheme uri))))
	(header `((host . ,host) ,@header))
	(request
	 (make-http-client-request
	  (make-uri '() '() (uri-path uri) (uri-query uri) #f)
	  method
	  http-version
	  header
	  body)))
    (with-tcp-client 
     (list server-address: host
	   port-number: port
	   eol-encoding: 'cr-lf
	   buffering: #f
	   )
     (lambda (port) 
       (write-http-request request port)
       (force-output port) 
       (read-http-response port read)))))
