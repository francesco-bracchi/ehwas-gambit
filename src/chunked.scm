;;; TODO: test! test! test! never tested!
(##namespace ("ehwas-chunked#"))

(##include "~~/lib/gambit#.scm")
(include "utils#.scm")
(include "http-message#.scm")

(define chunk-size (make-parameter 4096))

(define (with-chunked-writer writer)
  (lambda (#!optional 
	   what
	   (header '()) 
	   (port (current-input-port))
	   (encode identity))
    (if (equal? (assq 'transfer-encoding header) "chunked")
	(chunked-write what writer header port encode))))

(define (with-chunked-reader reader)
  (lambda (#!optional 
	   (header '()) 
	   (port (current-input-port))
	   (encode identity))
    (if (equal? (assq 'transfer-encoding header) "chunked")
	(chunked-read reader header port encode))))

(define (chunked-read reader header port encode)
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
	     (reader header server encode))))

(define (chunked-write what writer header port encode)
  (let((buffer (make-u8vector (chunk-size))))
    (receive (client server) (open-u8vector-pipe)
	     (thread-start! (make-thread (lambda () (writer what header client encode))))
	     (let produce ()
	       (let(
		    (len (read-subu8vector buffer 0 (chunk-size) server)))
		 (display (number->string len 16) port)
		 (if (> n 0)
		     (begin
		       (newline port)
		       (write-subu8vector buffer 0 len port)
		       (produce))))))))

(current-http-reader 
 (let*(
       (normal-reader (current-http-reader))
       (chunked-reader (with-chunked-reader normal-reader)))
   (lambda (#!optional 
	    (header '()) 
	    (port (current-input-port))
	    (encode identity))
     (if (equal? (assh header 'content-encoding) "chunked")
	 (chunked-reader header port encode)
	 (normal-reader header port encode)))))

(current-http-writer
 (let*((normal-writer (current-http-writer))
       (chunked-writer (with-chunked-writer normal-writer)))
   (lambda (what
	    #!optional 
	    (header '()) 
	    (port (current-input-port))
	    (encode identity))
     (if (equal? (assh header 'content-encoding) "chunked")
	 (chunked-writer what header port encode)
	 (normal-writer what header port encode)))))
