(##namespace ("ehwas-test#"))
(##include "~~lib/gambit#.scm")
(include "~~ehwas/ehwas#.scm")

(define template0
  (template
   (top
    (html
     (head 
      (title ,"title"))
     (body
      (div (@ (class . "header"))
	   (h1 ,'(p "header"))))))))

(define template1
  `(top (= "<html><head><title>") 
	"title" 
	(= "</title></head><body><div class=\"header\"><h1>") 
	(p "header") 
	(= "</h1></div></body></html>")))

(define (test-template)
  (if (equal? template0 template1) 
      (display ".")
      (error "failed template"
	     `(expected ,template0)
	     `(found ,template1))))

(test-template)

