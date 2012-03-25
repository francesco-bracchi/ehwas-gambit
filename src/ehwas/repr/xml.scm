(##namespace ("ehwas-xml#"))

(##include "~~/lib/gambit#.scm")
(include "../../ansuz/sources/port#.scm")
(include "../../ansuz/char-stream-parser#.scm")
(include "../../ansuz/re#.scm")

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
;; 

(define (push elem stack)
  (cons (cons (caar stack) (cons elem (cdar stack)))
	(cdr stack)))

(define (xml-text stack type text)
  (push text stack))

(define (xml-pi stack name text)
  (push 
   (list '? name text)
   ;; (list '? (if (eq? name 'xml) (string->attributes text) text))
   stack))

(define (xml-comment stack name text)
  (push (list '!-- text) stack))

(define (xml-entity stack name text)
  (cond
   ((eq? name 'gt) (push ">" stack))
   ((eq? name 'lt) (push "<" stack))
   ((eq? name 'amp) (push "&" stack))
   (else
    (push (list '& name) stack))))

(define (xml-open stack name attributes)
  (cons 
   (if (null? attributes) 
       (list name)
       (list name (cons '@ attributes)))
   stack))

(define (merge-and-reverse lst)
  (if (null? lst) lst
      (let merge-and-reverse ((lst (cdr lst))
			      (rev (list (car lst))))
	(cond
	 ((null? lst) rev)
	 ((and (string? (car lst))
	       (string? (car rev)))
	  (merge-and-reverse (cdr lst) 
			     (cons (string-append (car rev) (car lst))
				   (cdr rev))))
	 (else
	  (merge-and-reverse (cdr lst)
			     (cons (car lst) rev)))))))
			  
(define (pop stack)
  (let*(
	(curr (car stack))
	(prev (cadr stack))
	(rest (cddr stack))
	(current (cons (car curr) (merge-and-reverse (cdr curr))))
	(previous (cons (car prev) (cons current (cdr prev)))))
    (cons previous rest)))

(define xml-strict (make-parameter #f))

(define (xml-close stack name nothing)
  (if (and (eq? (caar stack) name) (xml-strict))
      (pop stack)
      (let find ((pstack stack))
	(cond
	 ((null? pstack) 
	  stack)	 
	 ((eq? (caar pstack) name)
	  (pop pstack))
	 (else
	  (find (pop pstack)))))))

(define-parser (sxml)
  (<- stack (sax '((top))
		 xml-open xml-close
		 xml-text
		 xml-pi 
		 xml-comment
		 xml-entity))
  (return (car stack)))

;; WRITE PROCEDURES

(define (write-xml-expr sxml port cdata-elements #!optional (cdata #f))
  (cond
   ((string? sxml) (write-xml-string sxml port cdata))
   ((not (pair? sxml)) (print port: port (decode sxml)))
   ((eq? (car sxml) 'top) (write-xml-fragment sxml port cdata-elements cdata))
   ((eq? (car sxml) '=) (print port: port (cadr sxml)))
   ((eq? (car sxml) '?) (write-xml-pi sxml port))
   ((eq? (car sxml) '!--) (write-xml-comment sxml port))
   ((eq? (car sxml) '&) (write-xml-entity sxml port))
   (else (write-xml-node sxml port cdata-elements))))

(define (write-xml-fragment sxml port cdata-elements cdata)
  (for-each (lambda (child) (write-xml-expr child port cdata-elements cdata))
	    sxml))

(define (write-xml-string string port #!optional cdata)
  (if cdata
      (for-each-display port (list "<![CDATA[" string "]]>"))
      (for-each-display  port (map (lambda (ch)
				     (cond
				      ((eq? ch #\>) "&gt;")
				      ((eq? ch #\<) "&lt;")
				      ((eq? ch #\&) "&amp;")
				      (else ch)))
				   (string->list string))))
  (force-output port))

(define (write-xml-pi pi port)
  (for-each-display 
   port 
   (append 
    (list "<?" (cadr pi) " ")
    (cddr pi)
    (list "?>"))))

(define (write-xml-comment comment port)
  (for-each-display
   port
   (append 
    (list "<!--")
    (cdr comment) 
    (list "-->")))
  (force-output port))

(define (write-xml-entity ent port)
  (for-each-display
   port
   (append (list "&")
	   (cdr ent) 
	   (list ";"))))

(define (attribute-encode string)
  (call-with-output-string
   ""
   (lambda (port)
     (for-each-display 
      port
      (map (lambda (ch) (if (eq? ch #\") "\\\"" ch))
	   (string->list string))))))

(define (write-xml-node node port cdata-elements)
  (let*(
	(type (car node))
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
    (if (null? childs)
	(display "/>" port)
	(begin
	  (display ">" port)
	  (write-xml-fragment childs port cdata-elements cdata)
	  (for-each-display port (list "</" type ">"))))
    (force-output port)))

(define (for-each-display port data)
  (for-each (lambda (x) (display x port))
	    data))

(define (read-xml header port)
  (let((result (run (sxml) port)))
    (cons (car result) (reverse (cdr result)))))

(define current-cdata-elements (make-parameter '(code script)))

(define (write-xml sxml header port)
  (cond
   ((u8vector? sxml) (write-subu8vector sxml 0 (u8vector-length sxml) port))
   ((procedure? sxml) (sxml port))
   ((string? sxml) (display sxml port))
   (else
    (for-each (lambda (e) (write-xml-expr e port cdata-elements))
              (cdr sxml)))))

(define (xml->string data #!optional (cdata-elements '()) (cdata #f))
  (call-with-output-string 
   "" 
   (lambda (port) 
     (write-xml-fragment data port cdata-elements cdata))))

(http-reader-set! "text/xml" read-xml)
(http-writer-set! "text/xml" write-xml)
