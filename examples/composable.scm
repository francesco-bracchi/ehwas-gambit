(include "~~ehwas/ehwas#.scm")
(include "shift-reset#.scm")
(load "src/http-server")

(define current-resume-table (make-parameter (make-table init: #f)))

(define (resume-link table request)
  (let*((path (list->link (http-request-path request)))
	(handler (table-ref table path #f)))
    (and handler (handler request))))

(define (with-resume handler #!optional (table (current-resume-table)))
  (lambda (request)
    (or (resume-link table request)
	(reset (handler request) (thread-terminate! (current-thread))))))

(define (list->link list)
  (if (null? list) ""
      (string-append "/" (symbol->string (car list)) (list->link (cdr list)))))

(define (snoc es e)
  (reverse (cons e (reverse es))))

(define (call/link handler
		   #!key
		   (table (current-resume-table))
		   (prefix '())
		   (digits 10)
		   (uid (make-uid digits))
		   (link (list->link (snoc prefix uid))))
  (table-set! table link handler)
  link)
   
(define-macro (link formals . body)
  (let((request (gensym 'request)))
    (cond
     ((null? formals)
      `(call/link (lambda (,request) ,@body)))
     ((pair? formals)
      `(call/link (lambda (,request) (with-fields ,request ,formals ,@body))))
     ((symbol? formals)
      `(call/link (lambda (,request) (let ((,formals (request-query 'request))) ,@body))))
     (else 
      (error "link syntax error")))))

(define (sum request)
  (let sum ((v0 0) (vs '()))
    (let((v (get-value v0 vs)))
      (if v (sum (+ v0 v) (append vs (list v)))))))

(define (get-value v0 vs)
  (shift resume 
	 (http-response 
		 body: (page-template 
			title: (string-append "Sum " (number->string v0))
			body: (template (div ,(form-template resume) ,(table vs)))
			footer: (template (p "total is " ,v0))))))

(define (table vs)
  (template
   (table ,(map (lambda (v) (template (tr (td "+") (td (@ (style "text-align:right;width:250px")) ,v)))) vs)
	  (tr (@ (style "background:#ff0")) (td "total:") (td ,(apply + vs))))))

(define (page-template #!key title body footer)
  (template
   (html 
    (head (title ,title))
    (body 
     (div (@ (class . "body")) ,body)
     (div (@ (class  . "footer")) ,footer)))))

(define (form-template resume)
  (template
   (div (@ (class . "template"))
	(form (@ (action . ,(link (val) (resume (string->number val)))))
	      (label "Add: " (input (@ (type . "text") (name . "val") (value . "0")))))
	(ul (li (a (@ (href . ,(link () (resume 1)))) "increment (+1)"))
	    (li (a (@ (href . ,(link () (resume -1)))) "decrement (-1)"))))))

(http-service-register! (with-resume sum) port-number: 6080)
(thread-sleep! +inf.0)
