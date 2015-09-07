(##namespace  ("ehwas-utils#"))

(##include "~~lib/gambit#.scm")

(include "~~ansuz/on-strings#.scm")
(include "~~ansuz/re#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (fixnum))

(define (identity x) x)

;; for-each assoc, same as for-each except that destructures alist pairs
(define (for-each-assoc fun alist)
  (for-each (lambda (key-value)
	      (fun (car key-value) (cdr key-value)))
	    alist))

(define (map-assoc fun alist)
  (map (lambda (key-value)
	 (fun (car key-value) (cdr key-value)))
       alist))

(define (assh symbol header)
  (let assh ((header header))
    (cond
     ((null? header) #f)
     ((and (pair? (car header)) (eq? (caar header) symbol))
      (cdar header))
     (else 
      (assh (cdr header))))))

(define (string-downcase str)
  (list->string
   (map char-downcase 
	(string->list str))))

(define (memoize1 func #!key (test eq?))
  (let((*fail* (list 'fail))
       (*memo* (make-table test: test weak-keys: #t init: #f)))
    (lambda (key)
      (let((val (table-ref *memo* key *fail*)))
	(if (eq? val *fail*)
	    (let((val (func key)))
	      (table-set! *memo* key val)
	      val)
	    val)))))

(define-regexp space "[ \n\t]*")

(define-regexp name "[~;,=\" \n\t]+")

(define-regexp comma "[ \n\t]*,[ \n\t]*")

(define-regexp semicolumn "[ \n\t]*;[ \n\t]*")

(define-parser (quoted-string) 
  #\"
  (<- string (many (cat (alt #\\ (any))
			(get-if (lambda (ch) (not (eq? ch #\")))))))
  #\"
  (ret (list->string string)))

(define-parser (field-value)
  (alt (cat (<- first (token))
	    (<- rest (many (cat (comma) (token))))
	    (space) 
	    (eos)
	    (ret (cons first rest)))
       (ret #f)))

(define-parser (token)
  (<- token-name (alt (quoted-string) (name)))
  (space)
  (<- token-attributes (attributes))
  (space)
  (ret (if (null? token-attributes) 
	   token-name
	   (cons token-name token-attributes))))

(define-parser (attributes)
  (many (cat (semicolumn) (attribute))))

(define-parser (attribute)
  (<- key (alt (quoted-string) (name)))
  (space)
  (char #\=)
  (space)
  (<- value (alt (quoted-string) (name)))
  (ret (cons (string->symbol key) value)))


(define (parse-field-value value)
  (or (run (field-value) value)
      (list value)))

(define (parse-header-fields header)
  (map-assoc (lambda (key value)
	       (cons key (parse-field-value value)))
	     header))

(define *html-empty-types* '(br hr))

(define (make-uid #!optional (digits 10))
  (string->symbol
   (list->string
    (random-list digits))))

(define (random-list n)
  (let random-list ((n n) (rs '()))
    (if (= n 0) rs
	(random-list (- n 1) (cons (random-char) rs)))))

(define *chars* "0123456789abcdefgjkhilmnopqrtuvxywzABCDEFGJKHILMNOPQRTUVXYWZ")

(define (random-char)
  (string-ref *chars* (random-integer (string-length *chars*))))

(define current-resume-table (make-parameter (make-table init: #f)))
