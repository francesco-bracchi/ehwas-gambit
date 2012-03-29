;; a response is a structure that encapsulates the return value of
;; a request
;; resolver :: request -> response
;; it has 5 fields:
;; version: the http protocol version
;; code: the response code (normally 200, not found 404, et cetera)
;; status: the response status (normally OK, Not found ...)
;; header: an hash table containing response headers,
;;         keys and values should be strings
;; Author Francesco Bracchi

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
         ;; (not safe)
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
    (print port: port (list "HTTP/" ,(car version) "." (cdr version) " " code " " status))
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


