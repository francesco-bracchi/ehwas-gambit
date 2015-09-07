(##namespace ("rfc822#"))
 
(##include "~~lib/gambit#.scm")

(include "~~ansuz/sources/port#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")

(include "utils#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe)
	 (not inline)
         (fixnum))
  
(define-regexp space "[\t ]*")

(define-regexp field-key "[~:]*")

(define-parser (field-value)
  (<- tokens (field-value-more '()))
  (ret (apply string-append tokens)))

(define-parser (field-value-more tokens)
  (cat (<- token (regexp "[~\n]*"))
       (let((tokens (cons token tokens)))
	 (alt (field-value-continue tokens)
	      (field-value-end tokens)))))

(define-parser (field-value-continue tokens)
  (cat (regexp "\n[\t ]")
       (field-value-more tokens)))

(define-parser (field-value-end tokens)
  (cat #\newline (ret (reverse tokens))))

(define-parser (rfc822)
  (rfc822-more '()))

(define-parser (rfc822-more alst)
  (alt (cat #\newline
	    (ret (reverse alst)))
       (cat (<- key-value (field))
	    (rfc822-more (cons key-value alst)))))

(define-parser (field)
  (<- k (field-key))
  #\:
  (space)
  (<- v (field-value))
  (ret (cons (string->symbol (string-downcase k)) v)))

(define (read-rfc822 #!optional (port (current-input-port)))
  (run (rfc822) port))

(define (print-value-list value #!optional (port (current-input-port)))
  (print-value (car value) port)
  (for-each (lambda (val) 
	      (print port: port ", ")
	      (print-value val port))
	    (cdr value)))

(define (print-value value #!optional (port (current-input-port)))
  (if (pair? value)
      (begin
	(print port: port (car value))
	(for-each (lambda (attr) 
		    (print port: port 
			   (if (pair? attr)
			       (list "; " (car attr) "=" (cdr attr))
			       attr)))
		  (cdr value)))
      (print port: port value)))


(define (write-rfc822 header #!optional (port (current-output-port)))
  (for-each-assoc (lambda (key value)
		    (print port: port (list key ": "))
		    (if (pair? value)
			(print-value-list value port)
			(print port: port value))
		    (newline port))
		  header))
