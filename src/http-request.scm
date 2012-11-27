;;; http-request.scm
(##namespace ("ehwas-request#"))

(##include "~~/lib/gambit#.scm")

(include "~~ansuz/sources/port#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")

(include "utils#.scm")
(include "rfc822#.scm")
(include "rfc3986#.scm")
(include "http-message#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe)
	 (not inline)
         (fixnum))

(define-http-message http-request 
  id: 54BE5195-EE7B-457C-826F-862BEB420747
  (method read-only:)
  (uri read-only:))

(define-regexp http-method "GET|POST|OPTIONS|HEAD|PUT|DELETE|TRACE|CONNECT")

(define-regexp space " *")

(define-parser (http-request-parser port read)
  (<- method (http-method))
  (space)
  (<- uri (rfc3986))
  (space)
  (<- version (http-version-parser))
  (space)
  #\newline
  (<- header (rfc822))
  (ret
   (let*((header (parse-header-fields header))
         (method (string->symbol method))
	 (body (read header port)))
     (make-http-request version header body method uri))))

(define (read-http-request #!optional 
			   (port (current-input-port))
			   (read (current-http-reader)))
  (run (http-request-parser port read) port))

(define (write-http-request request #!optional 
			    (port (current-output-port))
			    (write (current-http-writer)))
  (let*((method (http-request-method request))
	(version (http-message-version request))
	(uri (http-request-uri request))
	(header (http-message-header request))
	(body (http-message-body request)))
    (display method port)
    (display " " port)
    (if (string? uri) 
    	(display uri port) 
    	(write-uri uri port))
    (print port: port (list " HTTP/" (car version) "." (cdr version) " "))
    (newline port)
    (write-rfc822 header port)
    (newline port)
    (write body header port)))

(define (string->uri str)
  (call-with-input-string str read-uri))

(define (http-request #!key 
		      (method 'GET)
		      (uri "/")
		      (version (current-http-version))
		      (header '())
		      (body #f))
  (make-http-request
   version
   header
   body
   method
   (cond
    ((uri? uri) uri)
    ((string? uri) (string->uri uri))
    (else (uri)))))

;;; from http-message
(define http-request-version http-message-version)
(define http-request-header http-message-header)
(define http-request-body http-message-body)

(define (http-request-path request)
  (uri-path (http-request-uri request)))
