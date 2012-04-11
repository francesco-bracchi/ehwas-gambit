(##namespace ("ehwas-test#"))
(##include "~~lib/gambit#.scm")
(include "~~ehwas/ehwas#.scm")

(define string0
  "<html><head><title>title</title></head><body><div class=\"header\"><h1>header</h1></div></body></html>")

(define sexp0
  '(top
    (html
     (head 
      (title "title"))
     (body
      (div (@ (class . "header"))
	   (h1 "header"))))))

(define (test-write-html)
  (let((string1 (with-output-to-string "" (lambda () (write-html sexp0)))))
    (if (string=? string1 string0)
	(display ".")
	(error "failed test writing html" 
	       `(expected ,string0)
	       `(found ,string1)))))

(define (test-read-html) 
  (let((sexp1 (with-input-from-string string0 read-html)))
    (if (equal? sexp1 sexp0)
	(display ".")
	(error "failed test writing html" 
	       `(expected ,sexp0)
	       `(found ,sexp1)))))  

(test-write-html)
(test-read-html)
