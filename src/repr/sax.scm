;; Implement IPV6 !!! and ipvfuture

(##namespace ("ehwas-sax#"))

(##include "~~/lib/gambit#.scm")
(include "~~ansuz/on-ports#.scm")
(include "~~ansuz/re#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
	 (not inline)
         (not safe)
         (fixnum))

(define-regexp xml-space "[\x20\x9\xD\xA]+")

(define-regexp maybe-xml-space "[\x20\x9\xD\xA]*")

(define-regexp xml-name-string "[:_a-zA-Z\xC8-\x10000][:_a-zA-Z0-9\xC8-\\.\\-\\xB7\x10000]*")

(define-parser (xml-name)
  (<- r (xml-name-string))
  (ret (string->symbol r)))

(define-parser (xml-entity state handler)
  #\&
  (<- name (xml-name))
  #\;
  (ret (handler state name #f)))

(define-regexp xml-char-entity-decimal "[0-9]+")

(define-regexp xml-char-entity-hex "[0-9A-Fa-f]+")

(define-parser (xml-char-entity state handler)
  #\& #\#
  (<- code 
      (alt (cat (<- s (xml-char-entity-decimal))
		#\;
		(ret (string->number s)))
	   (cat #\x
		(<- s (xml-char-entity-hex))
		#\;
		(ret (string->number s 16)))))
  (ret 
   (handler 
    state 
    '*ENTITY*
    (string (integer->char code)))))

(define-parser (xml-text state handler)
  (reflect (ts sc fl)
	   (let((c (stream-car ts)))
	     (if (or (eof-object? c) (char=? c #\<) (char=? c #\&))
		 (fl '(empty text) ts sc)
		 (let loop ((ts ts)
			    (b (make-string 2048))
			    (p 0)
			    (bs '()))
		   (let((c (stream-car ts)))
		     (cond
		      ((or (eof-object? c) (char=? c #\<) (char=? c #\&))
		       (sc (handler
			    state
			    '*TEXT*
			    (apply string-append (reverse (cons (substring b 0 p) bs))))
			   ts fl))
		      ((>= p (string-length b))
		       (let((b1 (make-string (* 2 (string-length b)))))
			 (string-set! b1 0 c)
			 (loop (stream-cdr ts) b1 0 (cons b bs))))
		      (else
		       (string-set! b p c)
		       (loop (stream-cdr ts) b (+ p 1) bs)))))))))

(define-regexp cdata-open "<!\\[CDATA\\[")

(define-regexp cdata-text "([~\\]]|\\][~\\]]|\\]\\][~>])*")

(define-regexp cdata-close "\\]\\]>")

(define-parser (xml-cdata state handler)
  (cdata-open)
  (<- text (cdata-text))
  (cdata-close)
  (ret (handler state '*CDATA* text)))

(define-regexp xml-comment-text "([~\\-]|\\-[~\\-])*")

(define-parser (xml-comment state handler)
  #\< #\! #\- #\-
  (<- text (xml-comment-text))
  #\- #\- #\>
  (ret (handler state '*COMMENT* text)))

(define-regexp xml-pi-text "([~\\?]|\\?[~>])*")
(define-parser (xml-pi state handler)
  #\< #\?
  (<- name (xml-name))
  (xml-space)
  (<- text (xml-pi-text))
  #\? #\>
  (ret (handler state name text)))

;; (define-parser (xml-attribute-value)
;;   #\"
;;   (<- text (xml-attribute-text))
;;   #\"
;;   (ret text))

(define-parser (xml-attribute-value)
  (reflect (ts sc fl)
	   (let((data (make-string 1024))
		(c (stream-car ts)))
	     (if (eq? c #\")
		 (let loop ((ts (stream-cdr ts)) (j 0))
		   (let((c (stream-car ts)))
		     (cond
		      ((eq? c #\")
		       (sc (substring data 0 j) (stream-cdr ts) fl))
		      ((eq? c #\\)
		       (let*((ts (stream-cdr ts))
			     (c (stream-car ts)))
			 (if (eof-object? c) (fl 'eof ts sc)
			     (begin
			       (string-set! data j c)
			       (loop (stream-cdr ts) (+ j 1))))))
		      ((char? c)
		       (string-set! data j c)
		       (loop (stream-cdr ts) (+ j 1)))
		      (else
		       (fl 'eof ts sc)))))
		 (fl 'dc ts sc)))))

(define-parser (xml-attribute)
  (<- name (xml-name))
  (maybe-xml-space)
  #\=
  (maybe-xml-space)
  (<- val (xml-attribute-value))
  (ret (cons name val)))

(define-parser (xml-attributes)
  (many (cat (xml-space) (xml-attribute))))

(define-parser (xml-open state open close)
  #\<
  (<- name (xml-name))
  (<- as (xml-attributes))
  (maybe-xml-space)
  (alt (cat #\/ #\>
	    (ret (close (open state name as) name '())))
       (cat #\>
	    (ret (open state name as)))))

(define-parser (xml-close state handler)
  #\< #\/
  (<- name (xml-name))
  (maybe-xml-space)
  #\>
  (ret (handler state name '())))

(define-parser (sax-element state on-open on-close on-text on-pi on-comment on-entity)
  (alt (xml-open state on-open on-close)
       (xml-close state on-close)
       (xml-text state on-text)
       (xml-comment state on-comment)
       (xml-pi state on-pi)
       (xml-cdata state on-text)
       (xml-char-entity state on-text)
       (xml-entity state on-entity)))

(define-parser (sax state on-open on-close on-text on-pi on-comment on-entity)
  (alt (cat (<- state (sax-element state 
				   on-open 
				   on-close 
				   on-text 
				   on-pi 
				   on-comment 
				   on-entity))
	    (sax state on-open on-close on-text on-pi on-comment on-entity))
       (ret state)))
