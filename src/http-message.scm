(##namespace ("ehwas-http-message#"))

(##include "~~/lib/gambit#.scm")

(include "~~ansuz/sources/port#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")

(include "utils#.scm")
(include "rfc822#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe)
         (fixnum)
	 (not inline))

(define-type http-message
  extender: define-http-message
  id: 209121D0-95EA-4061-8C83-5665BBEDAAA3
  (version read-only:)
  (header read-only: unprintable:)
  (body read-only: unprintable:))

(define (get-content-type header)
  (let(
       (type (assh 'content-type header)))
    (cond
     ((and (pair? type) (pair? (car type))) (caar type))
     ((pair? type) (car type))
     (else type))))

(define (read-text #!optional 
		   (header '()) 
		   (port (current-input-port)))
  (read-line port #f))

(define (write-text str
		    #!optional
		    (header '())
		    (port (current-output-port)))
  (display str port))

(define *body-reader* (make-table))

(define *body-writer* (make-table))

(define (get-port header port)
  port)

(define current-http-reader 
;; todo handle chunked data
  (make-parameter 
   (lambda (header port)
     ((table-ref *body-reader* (get-content-type header) get-port) header port))))
      
  
(define current-http-writer 
  (make-parameter 
   (lambda (body header port)
     (cond
      ((table-ref *body-writer* (get-content-type header) #f) =>
       (lambda (writer) (writer body header port)))
      ((not body) 'OK)
      ((procedure? body) (body port))
      ((u8vector? body) (write-subu8vector body 0 (u8vector-length) port))
      ((string? body) (display body port))
      (else (error "Unkonwn type" body))))))

(define (http-reader-set! type fun)
  (table-set! *body-reader* type fun))

(define (http-writer-set! type fun)
  (table-set! *body-writer* type fun))

(define (http-reader type)
  (table-ref *body-reader* type))

(define (http-writer type)
  (table-ref *body-writer* type))

(define current-http-version (make-parameter '(1 . 1)))
  
(http-reader-set! "text/plain" read-text)
(http-writer-set! "text/plain" write-text)

(define digit->number
  (let(
       (d0 (char->integer #\0)))
    (lambda (v)
      (- (char->integer v) d0))))

(define-parser (http-version-parser)
  (<> (>> (regexp "HTTP/")
	  (<- mj (digit))
	  (char #\.)
	  (<- mn (digit))
	  (return (cons (digit->number mj)
			(digit->number mn))))
      (return '(1 . 0))))


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
  (get #\newline)
  (<- header (rfc822))
  (return
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

;;; http-response.scm
(##namespace ("ehwas-response#"))
(##include "~~/lib/gambit#.scm")

(include "~~ansuz/sources/port#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")

(include "utils#.scm")
(include "rfc822#.scm")
(include "http-message#.scm")

;; (include "request#.scm")

(declare (standard-bindings)
         (extended-bindings)
	 (not inline)
         (not safe)
	 (fixnum)
         (block))

(define-http-message http-response 
  id: 8637C5FF-0CEA-4645-B05E-1243BE9BEAD8
  code 
  status)

(define-regexp sp " *")

(define-parser (http-response-parser port read)
  (<- version (http-version-parser))
  (sp)
  (<- code (regexp "[0-9][0-9][0-9]"))
  (sp)
  (<- status (regexp "[~\n]*"))
  (get #\newline)
  (<- header (rfc822))
  (return (let(
	       (code (string->number code))
	       (status (string->symbol status))
	       (header (parse-header-fields header))
	       (body (read header port))) 
	    (make-http-response
	     version
	     header
	     body
	     code 
	     status))))

(define (read-http-response #!optional 
			    (port (current-input-port))
			    (read (current-http-reader)))
  (run (http-response-parser port read) port))

(define (write-http-response response #!optional 
			     (port (current-output-port))
			     (write (current-http-writer)))
  (let((version (http-message-version response))
       (code (http-response-code response))
       (status (http-response-status response))
       (header (http-message-header response))
       (body (http-message-body response)))
    ;; (forr-each (lambda (e) (display e port))
    ;;            (list "HTTP/" (car version) "." (cdr version) " " code " " status))
    (print port: port (list "HTTP/" (car version) "." (cdr version) " " code " " status))
    (newline port)
    (write-rfc822 header port)
    (newline port)
    (write body header port)))

(define (http-response #!key
		       (version (current-http-version))
		       (code 200)
		       (status (default-response-status code))
		       (header '((content-type . "text/html")))
		       (body ""))
  (make-http-response version header body code status))

;; from http-message
(define http-response-version http-message-version)
(define http-response-header http-message-header)
(define http-response-body http-message-body)

(define (default-response-status code)
  ;; TODO extend this and hopefully use a vector/hashtable
  (cond
   ((= code 200) "OK")
   ((= code 201) "Create")
   ((= code 301) "Redirect")
   ((= code 404) "Not Found")
   ((= code 500) "Internal Server Error")
   (else 200)))


