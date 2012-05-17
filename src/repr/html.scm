(##namespace ("ehwas-html#"))

(##include "~~/lib/gambit#.scm")
(include "~~ansuz/on-ports#.scm")
(include "~~ansuz/re#.scm")

(include "../http-message#.scm")
(include "sax#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;; (not safe)
         (block))

;; a bit involved, anyway
;; at the top of the stack you've got the current element, with childs and attributes in 
;; reverse order  (we mantain the name as the first element
;; when an element closes
;; put it in second position in the current preceding element

(define *html-empty-types* `(area base basefont br col frame hr img input isindex link meta param))

(define (push elem stack)
  (cons (cons (caar stack) (cons elem (cdar stack)))
	(cdr stack)))

(define (html-text stack type text)
  (push text stack))

(define (html-pi stack name text)
  (push 
   (list '? name text)
   stack))

(define (html-comment stack name text)
  (push (list '!-- text) stack))

;; TODO: add numeric entities
(define (html-entity stack name text)
  (cond
   ((eq? name 'gt) (push ">" stack))
   ((eq? name 'lt) (push "<" stack))
   ((eq? name 'amp) (push "&" stack))
   ((eq? name 'nbsp) (push " " stack))
   (else (push (list '& name) stack))))

(define (html-open stack name attributes)
  (let((stack1 (cons 
		(if (null? attributes) 
		    (list name)
		    (list name (cons '@ attributes)) )
		stack)))
    (if (memq name *html-empty-types*)
	(html-close stack1 name 'nothing)
	stack1)))

(define (pop stack)
  (let*((curr (car stack))
	(prev (cadr stack))
	(rest (cddr stack))
	(current (cons (car curr) (reverse (cdr curr))))
	(previous (cons (car prev) (cons current (cdr prev)))))
    (cons previous rest)))

(define html-strict (make-parameter #f))

(define (html-close stack name nothing)
  (if (and (eq? (caar stack) name) (html-strict))
      (pop stack)
      (let find ((pstack stack))
	(cond
	 ((null? pstack) stack)
	 ((eq? (caar pstack) name) (pop pstack))
	 (else (find (pop pstack)))))))

(define-parser (html)
  (<- stack (sax '((top))
		 html-open
		 html-close
		 html-text
		 html-pi 
		 html-comment
		 html-entity))
  (ret (car stack)))

;; WRITE PROCEDURES
(define (write-html-expr html port cdata-elements #!optional (cdata #f))
  (cond
   ((string? html) (write-html-string html port cdata))
   ((not (pair? html)) (print port: port html))
   ((eq? (car html) 'top) (write-html-fragment (cdr html) port cdata-elements cdata))
   ((eq? (car html) 'unquote) (print port: port (cadr html)))
   ((eq? (car html) '=) (print port: port (cadr html)))
   ((eq? (car html) '?) (write-html-pi html port))
   ((eq? (car html) '!--) (write-html-comment html port))
   ((eq? (car html) '&) (write-html-entity html port))
   ((symbol? (car html)) (write-html-node html port cdata-elements))
   (else (write-html-fragment html port cdata-elements cdata))))

(define (write-html-fragment frag port cdata-elements cdata)
  (for-each (lambda (child) (write-html-expr child port cdata-elements cdata))
	    frag))

(define (write-html-string string port #!optional cdata)
  (for-each-display  port (map (lambda (ch)
				 (cond
				  ((eq? ch #\>) "&gt;")
				  ((eq? ch #\<) "&lt;")
				  ((eq? ch #\&) "&amp;")
				  (else ch)))
			       (string->list string))))

(define (write-html-pi pi port)
  (for-each-display
   port
   (list "<?" (cadr pi) " " (caddr pi) "?>")))

(define (write-html-comment comment port)
  (for-each-display
   port
   (list "<!--" (cadr comment) "-->"))
  (force-output port))

(define (write-html-entity ent port)
  (for-each-display
   port
   (list "&" (cadr ent) ";")))

(define (attribute-encode string)
  (call-with-output-string
   ""
   (lambda (port)
     (for-each-display 
      port
      (map (lambda (ch) (if (eq? ch #\") "\\\"" ch))
	   (string->list string))))))

(define (write-html-node node port cdata-elements)
  (let*((type (car node))
	(second-element (and (pair? (cdr node)) (cadr node)))
	(attributes (if (and (pair? second-element) (eq? (car second-element) '@))
			(cdr second-element) '()))
	(childs (if (and (pair? second-element) (eq? (car second-element) '@))
		    (cddr node)
		    (cdr node)))
	(cdata (memq type cdata-elements)))
    (for-each-display port (list "<" type))
    (for-each-display 
     port
     (apply
      append
      (map (lambda (p) (list " " (car p) "=\"" (attribute-encode (cdr p)) "\""))
	   attributes)))
    (display ">" port)
    (if (not (memq type *html-empty-types*))
	(begin
	  (for-each (lambda (child) (write-html-expr child port cdata-elements cdata))
		    childs)
	  (for-each-display port (list "</" type ">"))))
    (force-output port)))

(define (for-each-display port data)
  (for-each (lambda (x) (display x port))
	    data))

(define (read-html #!optional (header '()) (port (current-input-port)))
  (let((result (run (html) port)))
    (cons (car result) (reverse (cdr result)))))

(define current-cdata-elements (make-parameter '(code)))

(define (write-html html #!optional 
		    (header '()) 
		    (port (current-output-port)))
  (cond
   ((u8vector? html) (write-subu8vector html 0 (u8vector-length html) port))
   ((string? html)  (display html port))
   ((procedure? html) (html port))
   ((not html) 'OK)
   (else (write-html-expr html port (current-cdata-elements)))))

(define (html->string html)
  (call-with-output-string "" (lambda (port) (write-html html port))))

(http-reader-set! "text/html" read-html)
(http-writer-set! "text/html" write-html)