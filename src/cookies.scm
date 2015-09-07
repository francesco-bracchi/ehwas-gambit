(##namespace  ("ehwas-cookies#"))

(##include "~~lib/gambit#.scm")

(include "utils#.scm")
(include "http-message#.scm")
(include "http-request#.scm")
(include "http-response#.scm")

(include "~~ansuz/sources/string#.scm")
(include "~~ansuz/re#.scm")

(include "~~ansuz/char-stream-parser#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
	 (not inline)
	 (not safe)
	 (fixnum)
         )

(define-regexp spaces " *")

(define-parser (cookie-field)
  (<- nam (regexp "[~= ]*"))
  (spaces) 
  #\=
  (spaces)
  (<- val (regexp "[~;\n]*"))
  (ret (cons (string->symbol nam) val)))

(define-parser (cookies)
  (<- first (cookie-field))
  (<- rest (many (cat (spaces) #\; (spaces) (cookie-field))))
  (ret (cons first rest)))

(define (really-request-cookies request)
  (let(
       (cookie-value (assq 'cookie (http-request-header request))))
    (and cookie-value
	 (parse-cookie (cadr cookie-value)))))

(define request-cookies (memoize1 really-request-cookies))

(define parse-cookie 
  (memoize1 (lambda (value) (run (cookies) value))))

