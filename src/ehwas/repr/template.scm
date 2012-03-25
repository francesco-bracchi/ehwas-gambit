(##namespace ("ehwas-template#"))

(##include "~~/lib/gambit#.scm")
(include "xml#.scm")
(include "html#.scm")
(include "../utils#.scm")
;; TEMPLATING

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;; (not safe)
         (block))

(define (variable? e)
  (and (pair? e) (eq? (car e) '$)))

(define (entity? e)
  (and (pair? e) (eq? (car e) '&)))

(define (pi? e)
  (and (pair? e) (eq? (car e) '?)))

(define (comment? e)
  (and (pair? e) (eq? (car e) '!)))

(define (top? e)
  (and (pair? e) (eq? (car e) 'top)))

(define (node? e)
  (and (pair? e) (symbol? (car e))))

(define (symbol->keyword s)
  (string->keyword
   (symbol->string s)))

(define (assk k lst)
  (let assk ((lst lst))
    (cond
     ((null? lst) #f)
     ((eq? (car lst) k) lst)
     (else (assk (cdr lst))))))

(define (template->function tpl)
  (lambda values
    (let run ((tpl tpl))
      (cond
       ((variable? tpl)
	(let (
	      (valk (assk (symbol->keyword (cadr tpl)) values)))
	  (or (and valk (cadr valk))
	      tpl)))
       ((pair? tpl) (cons (run (car tpl))
			  (run (cdr tpl))))
       (else tpl)))))

(define (run tpl . values)
  (apply (template->function tpl) values))

(define (reverse-and-coalesce rev)
  (if (null? rev) rev
      (let rev-and-coal ((rev (cdr rev))
			 (res (list (car rev))))
	(cond
	 ((null? rev) res)
	 ((and (string? (car rev))
	       (string? (car res)))
	  (rev-and-coal (cdr rev)
			(cons (string-append (car rev) (car res)) (cdr res))))
	 (else
	  (rev-and-coal (cdr rev)
			(cons (car rev) res)))))))
	  
(define (sanitize str)
  (apply string-append
	 (map (lambda (ch)
		(cond
		 ((eq? ch #\>) "&gt;")
		 ((eq? ch #\<) "&lt;")
		 ((eq? ch #\&) "&amp;")
		 (else (string ch))))
	      (string->list str))))
			
(define (disclose-node e fn)
  (let*(
	(second-element (and (pair? (cdr e)) (cadr e))))
    (fn
     (car e)
     (if (and (pair? second-element) (eq? (car second-element) '@))
	 (cdr second-element) '())
     (if (and (pair? second-element) (eq? (car second-element) '@))
	 (cddr e)
	 (cdr e)))))


;; simplify xml returns a template equivalent to tpl
;; but as a list of raw strings. i.e.
;; (hcard (name ($ name)) (address ($ address)))
;; is transformed in
;; ((= "<hcard><name>") 
;;  ($ name) 
;;  (= "</name><address>") 
;;  ($ address) 
;;  (= "</address></hcard>"))
;; that provide the same output passed through write-response
(define (simplify-xml tpl)
  
  (define stack '())
  
  (define (push! val) (set! stack (cons val stack)))
  
  (define (stringify tpl)
    (cond
     ((variable? tpl) 
      (push! tpl))

     ((string? tpl) 
      (push! (sanitize tpl)))

     ((symbol? tpl)
      (push! (xml->string (symbol->string tpl))))

     ((number? tpl)
      (push! tpl))
     
     ((entity? tpl) 
      (push! "&") 
      (for-each stringify (cdr tpl))
      (push! ";"))

     ((pi? tpl)
      (push! "<?")
      (push! (symbol->string (cadr tpl)))
      (push! " ")
      (push! (caddr tpl))
      (push! "?>"))

     ((comment? tpl)
      (push! "<!--")
      (for-each stringify (cdr tpl))
      (push! "-->"))
     
     ((node? tpl)
      (disclose-node 
       tpl
       (lambda (type attributes childs)
	 (push! "<")
	 (push! (symbol->string type))
	 (for-each (lambda (attribute)
		     (push! " ")
		     (push! (symbol->string (car attribute)))
		     (push! "=\"")
		     (stringify (cdr attribute))
		     (push! "\""))
		   attributes)
	(if (null? childs) (push! "/>")
	    (begin
	      (push! ">")
	      (for-each stringify childs)
	      (push! "</")
	      (push! (symbol->string type))
	      (push! ">"))))))))
  
  (if (top? tpl) 
      (for-each stringify (cdr tpl))
      (stringify tpl))
  (let(
       (result
	(map (lambda (el) 
	       (if (string? el) `(= ,(##still-copy el)) el))
	     (reverse-and-coalesce stack))))
    (if (top? tpl ) `(top ,@result) result)))

(define (simplify-html tpl)

  (define stack '())
  
  (define (push! val) (set! stack (cons val stack)))
  
  (define (stringify tpl)
    (cond
     ((variable? tpl) 
      (push! tpl))

     ((string? tpl) 
      (push! (sanitize tpl)))

     ((number? tpl)
      (push! tpl))

     ((symbol? tpl)
      (push! (xml->string (symbol->string tpl))))
     
     ((top? tpl)
      (for-each stringify (cdr tpl)))

     ((entity? tpl) 
      (push! "&") 
      (for-each stringify (cdr tpl))
      (push! ";"))

     ((pi? tpl)
      (push! "<?")
      (push! (symbol->string (cadr tpl)))
      (push! " ")
      (push! (caddr tpl))
      (push! "?>"))

     ((comment? tpl)
      (push! "<!--")
      (for-each stringify (cdr tpl))
      (push! "-->"))
     
     ((node? tpl)
      (disclose-node 
       tpl
       (lambda (type attributes childs)
	 (push! "<")
	 (push! (symbol->string type))
	 (for-each (lambda (attribute)
		     (push! " ")
		     (push! (symbol->string (car attribute)))
		     (push! "=\"")
		     (stringify (cdr attribute))
		     (push! "\""))
		   attributes)
	 (push! ">")
	 (if (not (memq type *html-empty-types*))
	     (begin
	       (for-each stringify childs)
	       (push! "</")
	       (push! (symbol->string type))
	       (push! ">"))))))))

  (if (top? tpl) 
      (for-each stringify (cdr tpl))
      (stringify tpl))
  
  (let(
       (result
	(map (lambda (el) 
	       (if (string? el) `(= ,(##still-copy el)) el))
	     (reverse-and-coalesce stack))))
    (if (top? tpl ) `(top ,@result) result)))

