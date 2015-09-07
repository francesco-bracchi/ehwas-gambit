(##namespace ("rfc822+#"))

(##include "~~lib/gambit#.scm")

(include "~~ansuz/sources/string#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")

(include "utils#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe)
	 (not inline)
         (fixnum))

(define-regexp space "[ \n\t]*")

(define-regexp name "[~;,=\" \n\t]+")

(define-regexp comma "[ \n\t]*,[ \n\t]*")

(define-regexp semicolumn "[ \n\t]*;[ \n\t]*")

(define-parser (quoted-string) 
  (cat #\"
       (<- string (many (alt (cat  #\\ (any)) 
			     (get-if (lambda (ch) (not (eq? ch #\")))))))
       #\"
       (ret (list->string string))))

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


(define (read-rfc822 #!optional (port (current-input-port)))
  (parse-header-fields 
   (rfc822#read-rfc822 port)))


(define (write-rfc822 header #!optional (port (current-output-port)))
  (rfc822#write-rfc822 
   (unparse-header-fields header) 
   port))
