;;; TODO: test! test! test! never tested!
(##namespace ("ehwas-chunked#"))

(##include "~~/lib/gambit#.scm")
(include "../utils#.scm")
(include "../http-message#.scm")

(define chunk-size (make-parameter 4096))

(define (with-chunked-writer writer)
  (lambda (what header port)
    (chunked-write what writer header port)))

(define (with-chunked-reader reader)
  (lambda (header port)
    (chunked-read reader header port)))

(define (chunked-read reader header port)
  (let(
       (buffer (make-u8vector (chunk-size))))
    (receive (client server) (open-u8vector-pipe)     
	     (thread-start!
	      (make-thread
	       (lambda () 
		 (let consume ()
		   (let*(
			 (len (string->number (read-line port) 16))
			 (data (make-u8vector len)))
		     (read-subu8vector data 0 len port)
		     (write-subu8vector data 0 len client))))))
	     (reader header server))))

(define (chunked-write what writer header port)
  (let((buffer (make-u8vector (chunk-size))))
    (receive (client server) (open-u8vector-pipe)
	     (thread-start! (make-thread (lambda () (writer what header client))))
	     (let produce ()
	       (let(
		    (len (read-subu8vector buffer 0 (chunk-size) server)))
		 (display (number->string len 16) port)
		 (if (> len 0)
		     (begin
		       (newline port)
		       (write-subu8vector buffer 0 len port)
		       (produce))))))))

(current-http-reader 
 (let*(
       (normal-reader (current-http-reader))
       (chunked-reader (with-chunked-reader normal-reader)))
   (lambda (header port)
     (if (equal? (assh 'content-encoding header) "chunked")
	 (chunked-reader header port)
	 (normal-reader header port)))))

(current-http-writer
 (let*((normal-writer (current-http-writer))
       (chunked-writer (with-chunked-writer normal-writer)))
   (lambda (what header port)
     (if (equal? (assh 'content-encoding header) "chunked")
	 (chunked-writer what header port)
	 (normal-writer what header port)))))

