(include "~~ehwas/ehwas#.scm")
(include "../src/repr/template#.scm")

(define (make-uid)
  (string->symbol
   (list->string
    (random-list 12))))
  
(define (random-list n)
  (let random-list ((n n) (rs '()))
    (if (= n 0) rs
	(random-list (- n 1) (cons (random-char) rs)))))

(define *chars* "0123456789abcdefgjkhilmnopqrtuvxywz")

(define (random-char)
  (string-ref *chars* (random-integer (string-length *chars*))))

(define current-callback-table (make-table))

(define current-return-table (make-table))

(define (current-return)
  (table-ref current-return-table (current-thread)))

(define (current-return-set! fn)
  (table-set! current-return-table (current-thread) fn))

(define (return! val)
  ((current-return) val))

(define (reply fn)
  (let((uid (make-uid)))
    (request-query
     (call/cc
      (lambda (cont)
	(table-set! current-callback-table uid cont)
	(pp (u8vector-length (object->u8vector cont)))
	(return! (fn (string-append "/" (symbol->string uid)))))))))

(define (with-continuation handler)
  (lambda (request)
    (call/cc 
     (lambda (return)
       (current-return-set! return)
       (or (resume-continuation request)
	   (handler request))))))

(define (resume-continuation request)
  (let*((path (http-request-path request)))
    (and (= (length path) 1)
	 (let((cont (table-ref current-callback-table (car path) #f)))
	   (and cont (cont request))))))

(define (page #!key (title "") (content "") (footer ""))
  (template
   (html
    (head (title ,title))
    (body
     (h1 (@ (class . "title")) ,title)
     (div (@ (class . "content")) ,content)
     (div (@ (class . "footer")) ,footer)))))

(define (get-term total)
  (reply (lambda (action)
	   (http-response
	    body: (page title: "calc"
			content: (template (form (@ (action . ,action) (method . "get"))
					   (input (@ (type "text") (name "term") (value "0")))))
			footer: (template ((p "subtotal is " (strong ,total)))))))))

(define (get-value total)
  (let*((term (get-term total))
	(number (cond
		 ((and term (assq 'term term)) => (lambda (p) (string->number (cdr p))))
		 (else #f))))
    (or number 0)))

(define (start-content)
  (let forever ((total 0))
    (let((term (get-value total)))
      (forever (+ total term)))))

(define (start request) (start-content))

(http-service-register! (with-continuation start)
 port-number: 6080)

(thread-sleep! 100000)