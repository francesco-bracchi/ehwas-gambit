(##namespace ("ehwas-http-message#"))

(##include "~~lib/gambit#.scm")

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

(define (get-content-type header)
  (let((type (assh 'content-type header)))
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

(define (default-http-reader header port)
  ((table-ref *body-reader* (get-content-type header) get-port) header port))

(define (default-http-writer body header port)
  (cond
   ((table-ref *body-writer* (get-content-type header) #f) =>
    (lambda (writer) (writer body header port)))
   ((not body) 'OK)
   ((procedure? body) (body port))
   ((u8vector? body) (write-subu8vector body 0 (u8vector-length) port))
   ((string? body) (display body port))
   (else (error "Unkonwn type" body))))

(define current-http-reader 
;; todo handle chunked data
  (make-parameter default-http-reader))

(define current-http-writer 
  (make-parameter default-http-writer))

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
  (let((d0 (char->integer #\0)))
    (lambda (v)
      (- (char->integer v) d0))))

(define-parser (http-version-parser)
  (alt (cat (regexp "HTTP/")
	    (<- mj (digit))
	    #\.
	    (<- mn (digit))
	    (ret (cons (digit->number mj)
		       (digit->number mn))))
       (ret '(1 . 0))))
