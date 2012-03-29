(##namespace ("rfc822+#"))


(##include "~~/lib/gambit#.scm")

(include "~~ansuz/sources/string#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")

(include "utils#.scm")

(define-regexp space "[ \n\t]*")

(define-regexp name "[~;,=\" \n\t]+")

(define-regexp comma "[ \n\t]*,[ \n\t]*")

(define-regexp semicolumn "[ \n\t]*;[ \n\t]*")

(define-parser (quoted-string) 
  (>> (get #\")
      (<- string (kleene (<> (>> (get #\\) (any))
			     (get-if (lambda (ch) (not (eq? ch #\")))))))
      (get #\")
      (return (list->string string))))

(define-parser (field-value)
  (<> (>> (<- first (token))
	  (<- rest (kleene (>> (comma) (token))))
	  (space) 
	  (eos)
	  (return (cons first rest)))
      (return #f)))

(define-parser (token)
  (<- token-name (<> (quoted-string) (name)))
  (space)
  (<- token-attributes (attributes))
  (space)
  (return (if (null? token-attributes) 
	      token-name
	      (cons token-name token-attributes))))

(define-parser (attributes)
  (kleene (>> (semicolumn) (attribute))))

(define-parser (attribute)
  (<- key (<> (quoted-string) (name)))
  (space)
  (char #\=)
  (space)
  (<- value (<> (quoted-string) (name)))
  (return (cons (string->symbol key) value)))

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
