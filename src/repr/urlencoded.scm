(##namespace ("ehwas-urlencoded#"))

(##include "~~/lib/gambit#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")
(include "~~ansuz/sources/port#.scm")

(include "../http-message#.scm")
(include "../rfc3986#.scm")
(include "../utils#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
	 (not safe)
	 (not inline)
         (fixnum))

(define-regexp urlencoded-token "[~\\&\\?\\=]+")
  
(define-parser (urlencoded-pair)
  (<- key (urlencoded-token))
  (get #\=)
  (<- value (urlencoded-token))
  (return (cons (string->symbol (pct-decode key))
		(pct-decode value))))

(define-parser (urlencoded)
  (<> (>> (<- first (urlencoded-pair))
	  (<- rest (kleene (>> (char #\&) (urlencoded-pair))))
	  (eos)
	  (return (cons first rest)))
      (return '())))

(define (u8vector->string vect)
  (call-with-input-u8vector vect
		       (lambda (p) (read-line p #f))))

(define (read-urlencoded #!optional
			 (header  '())
			 (port (current-input-port)))
  (let*(
	(size (string->number (car (assh 'content-length header))))
	(buff (make-u8vector size))
	(spur (read-u8 port))
	(read (read-subu8vector buff 0 size port)))
    (if (and  (= spur 10) (= read size)) ;; Investigate why we 've got this spurious #\linefeed char here
	(call-with-input-u8vector buff (lambda (p) (run (urlencoded) p)))
	(error "can't read urlencoded"))))

(define (write-urlencoded-pair pair #!optional (port (current-output-port)))
  (print port: port
	 (list (pct-encode (car pair))
	       "="
	       (pct-encode (cdr pair)))))

(define (write-urlencoded body 
			  #!optional 
			  (header '())
			  (port (current-output-port)))
  (if (u8vector? body)
      (write-subu8vector body 0 (u8vector-length body) port)
      (let foreach ((alist body))
	(cond
	 ((null? alist) 'OK)
	 ((null? (cdr alist))
	  (write-urlencoded-pair (car alist) port))
	 (else
	  (write-urlencoded-pair (car alist) port)
	  (display "&" port)
	  (foreach (cdr alist)))))))
  
(http-reader-set! "application/x-www-form-urlencoded" read-urlencoded)
(http-writer-set! "application/x-www-form-urlencoded" write-urlencoded)