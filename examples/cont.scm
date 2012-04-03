(include "~~ehwas/ehwas#.scm")
(include "../src/routes#.scm")

(define current-exit (make-parameter #f))
(define current-table (make-parameter #f))

(define *-max-* (expt 2 32))

(define (make-uid)
  (string->symbol (number->string (random-integer *-max-*) 16)))

(define (web-read formlet)
  (let((uid (make-uid)))
    (call/cc 
     (lambda (resume)
       (table-set! (current-table) uid resume)
       ((current-exit) (formlet uid))))))

(define (with-continuation handler #!key (prefix 'continue))
  (let((table (make-table)))
    (lambda (request)
      (let*((uri (request-uri request))
	    (path (uri-path uri)))
	(call/cc
	 (lambda (exit)
	   (parameterize
	    ((current-exit exit)
	     (current-table table))
	    (if (and (pair? path) (eq? (car path) prefix) (pair? (cdr path)))
		((table-ref table (cadr path)) request)
		(handler request)))))))))

(define-html page 
  (html
   (head (title ($ title)))
   (body
    (div (@ (class "header")) ($ header))
    (div (@ (class "body") ($ xxxx)))
    (div (@ (class "footer") ($ footer))))))

(pp page)

(define (sum request)
  (let loop ((n 0))
    (let((n0 (get-number n)))
      (loop (+ n n0)))))

(define (sum request)
  (let sum ((n 0))
    (sum (+ n (get-number n)))))

(define (get-number n)
  (let((data (web-read 
	      (lambda (uid)
		(page title: (string-append "got " (number->string n))
		      body: `(form (@ (action "/continue/" uid))
				   (input (@ (type "test") (name "val"))))
		      footer: '(h1 "ciao"))))))
    (cdr (assq 'val data))))

(define-handler (route me)
  (('GET ufo) 
   (let((body (page title: "ufo title" header: "ufo header" xxxx: "ufo body" footer: "ufo footer")))
     (pp body)
     (http-response body: body))))

(http-service-register! 
 route
 port-number: 6080)