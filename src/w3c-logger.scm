(##namespace ("ehwas-w3c-logger#"))

(##include "~~lib/gambit#.scm")

(include "http-server#.scm")
(include "http-request#.scm")
(include "http-response#.scm")
(include "rfc3986#.scm")

(declare
 (standard-bindings)
 (extended-bindings)
 ;; (not safe)
 ;; (block)
 )
 
(c-declare "#include <time.h>")

(define ctime (c-lambda () int "___result=time(NULL);"))

(define format-time
  (c-lambda (char-string long) char-string
#<<end-format-time

char res [80];
struct tm *t;
int status;

t=localtime (&___arg2);
status=strftime(res, 80, ___arg1, t);
if (! status) return ___FIX (___UNKNOWN_ERR);
___result= res;

end-format-time
))
                

(define (w3c-date ct)
  (declare (not interrupts-enabled))
  (format-time "%Y-%m-%d" ct))

(define (w3c-time ct)
  (declare (not interrupts-enabled))
  (format-time "%H:%M:%S" ct))

(define (datetime)
  (let(
       (now (ctime)))
    `(,(w3c-date now) " " ,(w3c-time now))))

(define (uri->string uri)
  (call-with-output-string
   (lambda (p)
     (write-uri uri p))))

(define (address->string a)
  (string-append
   (number->string (u8vector-ref a 0))
   "."
   (number->string (u8vector-ref a 1))
   "."
   (number->string (u8vector-ref a 2))
   "."
   (number->string (u8vector-ref a 3))))
                   
(define (log! handler fn)
  (let*(
	(lock (make-mutex))
	(wrp
	 (lambda (req)
	   (let(
		(res (handler req)))
	     (thread-start!
	      (make-thread
	       (lambda ()
		 (mutex-lock! lock)
		 (with-exception-catcher
		  (lambda (ex) 
		    (mutex-unlock! lock)
		    (raise ex))
		  (lambda ()
		    (append-to-file fn (print-log! req res))
		    (mutex-unlock! lock))))))
	     res))))
    (make-will wrp (lambda (_) (append-to-file fn print-footer!)))
    (append-to-file fn print-header!)
    wrp))

(define (print-header! p)
  (print
   port: p 
   `("#Software futhark+notapipe\n"
     "#Version 1.0\n"
     "#Start-Date " ,(datetime) "\n"
     "#Fields: "
     "date time "
     "c-ip "
     "cs-method cs-host cs-uri "
     "sc-code sc-status sc-bytes\n")))

(define (print-log! req res)
  (lambda (p)
    (print
     port: p
     `(,(datetime) " "
       ,(address->string (socket-info-address (tcp-client-peer-socket-info (current-input-port)))) " "
       ,(http-request-method req) " "
       ,(let(
	     (host (assoc 'Host (http-request-header req))))
	  (if host (cdr host) "-")) 
       " "
       ,(uri->string (http-request-uri req)) " "
       ,(http-response-code res) " "
       "\"" ,(http-response-status res) "\" "
       ,(let(
	     (len (assoc 'Content-Length (http-response-header res))))
	  (if len (cdr len) "-"))
       " \n"))))

(define (print-footer! p)
  (print
   port: p
   `("#End-Date " ,(datetime) "\n")))

(define (append-to-file fn proc)
  (call-with-output-file (list path: fn append: #t create: 'maybe) proc))
