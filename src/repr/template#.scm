(##namespace ("ehwas-template#" template))
	   
(define-macro (template e #!key 
			(mode 'html) 
			(autoclose '(area base basefont br col frame hr img input isindex link meta param)))
  
  (define (top? e)
    (and (pair? e) (eq? (car e) 'top)))

  (define (entity? e)
    (and (pair? e) (eq? (car e) '&)))
  
  (define (pi? e)
    (and (pair? e) (eq? (car e) '?)))
  
  (define (comment? e)
    (and (pair? e) (eq? (car e) '!)))
  
  (define (node? e)
    (and (pair? e) (symbol? (car e))))

  (define (unquote? e)
    (and (pair? e) (eq? (car e) 'unquote)))
  
  (define (node-type e)
    (car e))

  (define (node-attributes e)
    (let((sec (and (pair? (cdr e)) (cadr e))))
      (if (and (pair? sec) (eq? (car sec) '@))
	  (cdr sec) 
	  '())))  

  (define (node-children e)
    (let((sec (and (pair? (cdr e)) (cadr e))))
      (if (and (pair? sec) (eq? (car sec) '@))
	  (cddr e)
	  (cdr e))))
	  
  (define (sanitize str)
    (apply string-append
	   (map (lambda (ch)
		  (cond
		   ((eq? ch #\>) "&gt;")
		   ((eq? ch #\<) "&lt;")
		   ((eq? ch #\&) "&amp;")
		   (else (string ch))))
		(string->list str))))
  
  (define (concat . es)
    (let concat-list ((es es) (st '()))
      (cond
       ((null? es) (reverse st))
       ((string? (car es)) (concat-list (cdr es) (cons (car es) st)))
       ((unquote? (car es)) (concat-list (cdr es) (cons (car es) st)))
       ((null? (car es)) (concat-list (cdr es) st))
       ((pair? (car es)) (concat-list `(,(caar es) ,(cdar es) ,@(cdr es)) st))
       (else (error "concat failed" es)))))

  (define (flatten-node-xml n)
    (let((t (node-type n))
	 (as (node-attributes n))
	 (cs (node-children n)))
      (concat "<" (symbol->string t) 
	      (if (null? as) "" (concat " " (map flatten-attributes as)))
	      (if (null? cs) "/>" (concat ">" (map flatten cs) "</" (symbol->string t) ">")))))

  (define (flatten-node-html n)
    (let((t (node-type n))
	 (as (node-attributes n))
	 (cs (node-children n)))
      (concat "<" (symbol->string t) 
	      (if (null? as) "" (map flatten-attributes as))
	      (cond
	       ((and (eq? mode 'xml) (null? cs)) "/>")
	       ((and (eq? mode 'html) (null? cs) (memq t autoclose)) ">")
	       (else (concat ">" (map flatten cs) "</" (symbol->string t) ">"))))))

  (define (flatten-attributes a)
    (concat " " 
	    (symbol->string (car a))
	    "=\""
	    (flatten (cdr a))
	    "\""))
  
  (define (flatten e)
    (cond
     ((unquote? e) e)
     ((string? e) (sanitize e))
     ((char? e) (string e))
     ((number? e) (number->string e))
     ((entity? e) (concat ";" (map flatten (cdr e)) "&"))
     ((pi? e) (concat "<?" (symbol->string (cadr e)) " " (map flatten (cddr e)) "?>"))
     ((comment? e) (concat "<--" (map flatten (cdr e)) "-->"))
     ((node? e) (if (eq? mode 'html) 
		     (flatten-node-html e)
		     (flatten-node-xml e)))
     ((symbol? e) (symbol->string e))
     ((null? e) "")
     ((pair? e) (concat (flatten (car e)) (flatten (cdr e))))
     (else (error "template can't handle this" e))))
     
  
  (define (coalesce st)
    (if (not (pair? st)) st
	(let coalesce ((st (cdr st)) (cs (list (car st))))
	  (cond
	   ((null? st) (reverse cs))
	   ((and (string? (car st)) (string? (car cs)))
	    (coalesce (cdr st) (cons (string-append (car cs) (car st)) (cdr cs))))
	   (else (coalesce (cdr st) (cons (car st) cs)))))))
	
  (define (simplify e) 
    (coalesce
     (if (top? e) 
	 (apply concat (map flatten (cdr e)))
	 (flatten e))))
  
  (list 'quasiquote 
	(map (lambda (e) (if (string? e) `(= ,(##still-copy e)) e))
	     (if (top? e) `(top ,@(simplify e))
		 (simplify e)))))

(define-macro (html-frag . frag)
  `(template ,frag mode: 'html))

(define-macro (xml-frag . frag)
  `(template ,frag mode: 'xml))

(define-macro (xml . x)
  `(template
    (top ,@x)))

(define-macro (html . x)
  `(template
    (top (html ,@x))))

  