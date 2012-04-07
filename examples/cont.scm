(include "~~ehwas/ehwas#.scm")

(define (make-uid)
  (list->string
   (random-list 8)))

(define (random-list n)
  (let random-list ((n n) (rs '()))
    (if (= n 0) rs
	(random-list (- n 1) (cons (random-char) rs)))))

(define *chars* "0123456789abcdefgjkhilmnopqrtuvxywz")

(define (random-char)
  (string-ref *chars* (random-integer (string-length *chars*))))

(define current-callback-table (make-parameter (make-table init: #f)))

(define (populate-callbacks expr #!optional (table (current-callback-table)))
  (cond
   ((procedure? expr) 
    (let((uid (make-uid)))
      ;; (pp `(DATA ,(u8vector-length (object->u8vector expr))))
      (table-set! table uid expr)
      (string-append "/resume/" uid)))
   ((pair? expr)
    (let((kar (populate-callbacks (car expr)))
	 (kdr (populate-callbacks (cdr expr))))
      (cons kar kdr)))
   (else expr)))

(define (continue request #!optional (table (current-callback-table)))
  (let*((uri (http-request-uri request))
	(path (uri-path uri)))
    (and (pair? path) 
	 (eq? (car path) 'resume)
	 (pair? (cdr path))
	 (let*(
	       (name (symbol->string (cadr path)))
	       (callback (table-ref table name)))
	   (and (procedure? callback)
		(callback request))))))

(define (add-callbacks response #!optional (table (current-callback-table)))
  (make-http-response
   (http-message-version response)
   (http-message-header response)
   (populate-callbacks (http-message-body response) table)
   (http-response-code response)
   (http-response-status response)))


(define (with-callbacks handler #!optional (table (current-callback-table)))
  (lambda (request)
    (let((response
	  (or (continue request table)
	      (handler request))))
      (and response (add-callbacks response table)))))

(define-template (calc-tpl title number sum)
  `(html
   (head (title ,title))
   (body
    (table ,@(values or expr)
	   (
	 
(define-html calc-tpl
  (html
   (head (title "calculator"))
   (body 
    (table ($ values)
	   (tr (td (@ (style . "text-align:right")) ($ number)) (td "="))
	   (tr (td (@ (style . "text-align:right")) ($ sum))))
    (form (@ (method . "get")
	     (action . ($ action)))
	  (input (@ (type . "text") (name . "n") (value . "0")))
	  (input (@ (type . "submit") (value . "Sum")))))))

(define (calc request values)
  (pp (http-message-body request))
  (let*(
	(params (request-query request))
	(number (string->number (assh 'n params)))
	(vals (cons number values)))
    (http-response
     body: (calc-tpl
	    values: (simplify-html
		     `(top ,@(map (lambda (val) 
				    `(tr (td (@ (style . "text-align:right")) ,val)
					 (td "+")))
				  (reverse values))))
	    number: number
	    sum: (apply + vals)
	    action: (lambda (req) (calc req vals))))))

(define start
  (lambda (req)
    (let*(
	  (uri (http-request-uri req))
	  (path (uri-path uri)))
      (and (equal? path '(start)) 
	   (http-response
	    body:
	    `(top
	      (html
	       (head (title "calculator"))
	       (body 
		(h3 "Welcome to calculator, insert your first value")
		(form (@ (method . "get") 
			 (action . ,(lambda (req) (calc req '()))))
		      (input (@ (type . "text") (name . "n") (value . "0")))
		      (input (@ (type . "submit") (value . "first-value"))))))))))))

(define (on-error exc)
  (http-response
   code: 500
   status: "Internal Error"
   body: `(html
	   (head (title "Internal Error"))
	   (body
	    (h1 "Error 500")
	    (h3 "Internal Error")
	    (pre ,(call-with-output-string
		   "" 
		   (lambda (port)
		     (continuation-capture
		      (lambda (cont)
			(display-exception-in-context exc cont port)
			(display-continuation-backtrace cont port #f #t)
			;; (##repl-exception-handler-hook exc other-handler)
			)))))))))

(define (with-error handler #!optional (error-handler on-error))
  (lambda (request)
    (with-exception-catcher
     error-handler
     (lambda () (handler request)))))

(http-service-register! 
 (with-error (with-callbacks start))
 port-number: 6080)
