;;; TODO: test! test! test! never tested!
(##namespace ("ehwas-chunked#"))

(##include "~~lib/gambit#.scm")
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
  (receive (client server) (open-u8vector-pipe)
	   (let read ()
	     (let*((len (string->number (read-line port) 16))
		   (data (and len (make-u8vector len))))
	       (if (and len (> len 0))
		   (begin (read-subu8vector data 0 len port)
			  (write-subu8vector data 0 len client)
			  (force-output client)
			  (read))
		   (close-port client))))
	   (thread-join! (thread-start! (make-thread (lambda () (reader header server)))))))

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

(define (make-chunked-reader normal-reader)
  (let ((chunked-reader (with-chunked-reader normal-reader)))
    (lambda (header port)
      (if (equal? (assh 'transfer-encoding header) '("chunked"))
	  (chunked-reader header port)
	  (normal-reader header port)))))

  
(define (make-chunked-writer normal-writer)
  (let ((chunked-writer (with-chunked-writer normal-writer)))
    (lambda (what header port)
      (if (equal? (assh 'transfer-encoding header) '("chunked"))
	  (chunked-writer what header port)
	  (normal-writer what header port)))))

(current-http-reader (make-chunked-reader (current-http-reader)))
(current-http-writer (make-chunked-writer (current-http-writer)))

