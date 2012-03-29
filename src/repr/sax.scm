;; Implement IPV6 !!! and ipvfuture

(##namespace ("ehwas-sax#"))

(##include "~~/lib/gambit#.scm")
(include "~~ansuz/sources/port#.scm")
(include "~~ansuz/char-stream-parser#.scm")

(include "~~ansuz/re#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
	 (not inline)
         ;; (not safe)
         (fixnum))

(define-regexp xml-space "[\x20\x9\xD\xA]+")

(define-regexp maybe-xml-space "[\x20\x9\xD\xA]*")

(define-parser (xml-name)
  (<- r (regexp "[:_a-zA-Z\xC8-\x10000][:_a-zA-Z0-9\xC8-\\.\\-\\xB7\x10000]*"))
  (return (string->symbol r)))

(define-parser (xml-entity state handler)
  #\&
  (<- name (xml-name))
  #\;
  (return (handler state name #f)))

(define-parser (xml-char-entity state handler)
  (regexp "&#")
  (<- code 
      (<> (>> (<- s (regexp "[0-9]+"))
	      (get #\;)
	      (return (string->number s)))
	  (>> (get #\x)
	      (<- s (regexp "[0-9A-Fa-f]+"))
	      (get #\;)
	      (return (string->number s 16)))))
  (return 
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

(define-parser (xml-cdata state handler)
  (regexp "<!\\[CDATA\\[")
  (<- text (regexp "([~\\]]|\\][~\\]]|\\]\\][~>])*"))
  (regexp "\\]\\]>")
  (return (handler state '*CDATA* text)))

(define-parser (xml-comment state handler)
  (regexp "<!\\-\\-")
  (<- text (regexp "([~\\-]|\\-[~\\-])*"))
  (regexp "\\-\\->")
  (return (handler state '*COMMENT* text)))

(define-parser (xml-pi state handler)
  (regexp "<\\?")
  (<- name (xml-name))
  (xml-space)
  (<- text (regexp "([~\\?]|\\?[~>])*"))
  (regexp "\\?>")
  (return (handler state name text)))

  ;; BUG in expression (regexp "([~\n]|\\\\\")*")
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
  (get #\=)
  (maybe-xml-space)
  (<- val (xml-attribute-value))
  (return (cons name val)))

(define-parser (xml-attributes)
  (kleene (>> (xml-space) (xml-attribute))))

(define-parser (xml-open state open close)
  (get #\<)
  (<- name (xml-name))
  (<- as (xml-attributes))
  (maybe-xml-space)
  (<> (>> (word "/>")
            (return (close (open state name as) name '())))
      (>> (word ">")
	  (return (open state name as)))))

(define-parser (xml-close state handler)
  (word "</")
  (<- name (xml-name))
  (maybe-xml-space)
  (get #\>)
  (return (handler state name '())))

(define-parser (sax-element state on-open on-close on-text on-pi on-comment on-entity)
  (<> (xml-open state on-open on-close)
      (xml-close state on-close)
      (xml-text state on-text)
      (xml-comment state on-comment)
      (xml-pi state on-pi)
      (xml-cdata state on-text)
      (xml-char-entity state on-text)
      (xml-entity state on-entity)))

(define-parser (sax state on-open on-close on-text on-pi on-comment on-entity)
  (<> (>> (<- state (sax-element state 
				 on-open 
				 on-close 
				 on-text 
				 on-pi 
				 on-comment 
				 on-entity))
	  (sax state on-open on-close on-text on-pi on-comment on-entity))
      (return state)))
