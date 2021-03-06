;; Implement IPV6 !!! and ipvfuture

(##namespace ("rfc3986#"))

(##include "~~lib/gambit#.scm")
(include "~~ansuz/sources/port#.scm")
(include "~~ansuz/char-stream-parser#.scm")

(include "~~ansuz/re#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
	 (not inline)
         (not safe)
         (fixnum))

(define-structure uri 
  id: 58836E1A-FCD7-432D-A22E-5F0D40685324
  (scheme read-only:)
  (authority read-only:)
  (path read-only:)
  (query read-only:)
  (fragment read-only:))

;; (declare (not safe))

(define-regexp re-port "[0-9]{1,5}")

(define-parser (uri-port)
  (<- c (re-port))
  (let ((p (string->number c)))
    (if (and (> p 0) (< p 65536))
	(ret p)
        (fail "port number too high"))))

(define-regexp scheme "[a-zA-Z][a-zA-Z0-9\\+\\-\\.]*")

(define-regexp re-userinfo  "([a-zA-Z0-9!$&',;=:\\-\\.\\_\\~\\(\\)\\*\\+]|%[0-9a-fA-F]{2})*")

(define-parser (userinfo)
  (cat (<- c (re-userinfo))
       (ret (pct-decode c))))

(define (string-split str ch)
  (let for ((l0 0) (l1 0) (acc '()))
    (cond
     ((>= l1 (string-length str))
      (reverse (cons (substring str l0 l1) acc)))
     ((char=? (string-ref str l1) ch)
      (for (+ 1 l1) (+ 1 l1) (cons (substring str l0 l1) acc)))
     (else
      (for l0 (+ 1 l1) acc)))))

(define-regexp octet "((2[0-5][0-9]|1[0-9][0-9]|[0-9][0-9]|[0-9])\\.){3}(2[0-5][0-9]|1[0-9][0-9]|[0-9][0-9]|[0-9])")

(define-parser (ipv4)
  (cat (<- v (octet))
       (ret `(ip4 ,@(map string->number (string-split v #\.))))))

(define-regexp re-hostname  "([a-zA-Z0-9!$&',;=\\-\\.\\_\\~\\(\\)\\*\\+]|%[0-9a-fA-F]{2})*")

(define-parser (hostname)
  (cat (<- name (re-hostname))
       (ret (pct-decode name))))

(define-parser (host)
  (alt (ipv4)
       (hostname)))

(define-parser (authority)
  (cat (<- ui (alt (cat (<- u (userinfo))
			(char #\@)
			(ret u))
		   (ret #f)))
       (<- h (host))
       (<- p (alt (cat #\:
		       (<- p (uri-port))
		       (ret p))
		  (ret #f)))
       (ret (append
	     (if ui `((userinfo ,@ui)) '())
	     `((host ,@h))
	     (if p `((port ,@p)) '())))))

(define (string->segment str)
  (or (string->number str)
      (string->symbol str)))

(define-regexp re-segment "([a-zA-Z0-9\\-\\.\\_\\~!$&'\\(\\)\\*\\+,;=]|%[0-9a-fA-F]{2})*")

(define-parser (segment)
  (cat (<- r (re-segment))
       (ret (string->segment (pct-decode r)))))

;; (define-regexp re-segment-nz "([a-zA-Z0-9]|\\-|\\.|_|\\~|%[0-9a-fA-F]{2}|!|$|&|'|\\(|\\)|\\*|\\+|,|;|=)+")

(define-regexp re-segment-nz "([a-zA-Z0-9\\-\\.\\_\\~!$&'\\(\\)\\*\\+,;=]|%[0-9a-fA-F]{2})+")

(define-parser (segment-nz)
  (cat (<- r (re-segment-nz))
       (ret (string->segment (pct-decode r)))))

(define-regexp re-segment-nz-nc "([a-zA-Z0-9\\-\\.\\_\\~!$&'\\(\\)\\*\\+,;=:]|%[0-9a-fA-F]{2})+")

(define-parser (segment-nz-nc)
  (cat (<- r (re-segment-nz-nc))
       (ret (string->segment (pct-decode r)))))

(define-parser (path-abempty)
  (alt (cat #\/
	    (<- s (segment))
	    (<- ss (path-abempty))
	    (ret (cons s ss)))
       (ret '())))

(define-parser (path-absolute)
  #\/
  (alt (cat (<- s (segment-nz))
	    (<- ss (path-abempty))
	    (ret (cons s ss)))
       (ret '())))

(define-parser (path-noscheme)
  (cat (<- s (segment-nz-nc))
       (<- ss (path-abempty))
       (ret (cons s ss))))

(define-parser (path-rootless)
  (cat (<- s (segment-nz))
       (<- ss (path-abempty))
       (ret (cons s ss))))

(define-parser (path-empty)
  (ret '()))

(define-parser (path)
  (alt (path-abempty)
       (path-absolute)
       (path-noscheme)
       (path-rootless)
       (ret '())))

(define-regexp re-query "[a-zA-Z0-9\\-\\.\\_\\~!$&\\'\\(\\)\\*\\+\\,\\;=%@\\/\\?]*")

(define-parser (query)
  (alt (cat (char #\?) (re-query))
       (ret #f)))

(define-regexp re-fragment "([a-zA-Z0-9\\-\\.\\_\\~!$&\\'\\(\\)\\*\\+,;=\\/\\?]|%[0-9a-fA-F]{2})*")

(define-parser (fragment)
  (alt (cat #\#
	    (<- cs (re-fragment))
	    (ret (pct-decode cs)))
       (ret #f)))

(define-parser (absolute)
  (<- s (scheme))
  #\:
  (alt (cat #\/ #\/
	    (<- a  (authority))
	    (<- pa (path-abempty))
	    (<- q  (query))
	    (<- f  (fragment))
	    (ret (make-uri s a pa q f)))
       (cat (<- pa (alt (path-absolute)
			(path-rootless)
			(path-empty)))
	    (<- q (query))
	    (<- f (fragment))
	    (ret (make-uri s '() pa q f)))))

(define-parser (relative)
  (alt (cat #\/ #\/
	    (<- a  (authority))
	    (<- pa (path-abempty))
	    (<- q  (query))
	    (<- f  (fragment))
	    (ret (make-uri '() a pa q f)))
       (cat (<- pa (alt (path-absolute)
			(path-noscheme)
			(path-empty)))
	    (<- q (query))
	    (<- f (fragment))
	    (ret (make-uri '() '() pa q f)))))

(define-parser (rfc3986)
  (alt (absolute)
       (relative)))

(define (pct->u8vector s)
  (call-with-output-u8vector
   (u8vector)
   (lambda (out)
     (let for ((j 0))
       (if (>= j (string-length s)) 'ok
           (let(
                (c (string-ref s j)))
             (cond
              ((char=? c #\%)
	       (write-char (integer->char (string->number (substring s (+ j 1) (+ j 3)) 16)) out)
               (for (+ j 3)))
              ((char=? c #\+)
               (write-char #\space out)
               (for (+ j 1)))
              (else
               (write-char c out)
               (for (+ j 1))))))))))

(define (pct-decode s)
  (if (= 0 (string-length s)) ""
      (call-with-input-u8vector
       (list init: (pct->u8vector s)
             char-encoding: 'UTF-8)
       (lambda (p) 
	 (let(
	      (line (read-line p #f)))
	   line)))))

(define (string->u8vector string)
  (call-with-output-u8vector
   (u8vector)
   (lambda (port)
     (display string port))))

(define (char-code? j)
  (let(
       (ch (integer->char j)))
    (or (char-numeric? ch)
	(char-lower-case? ch)
	(char-upper-case? ch)
	(char=? ch #\.)
	(char=? ch #\=))))

(define (pct-encode str)
  (call-with-output-string
   ""
   (lambda (out)
     (let(
	  (vect (string->u8vector str)))
       (let for ((j 0))
	 (if (< j (u8vector-length vect))
	     (let(
		  (ch (u8vector-ref vect j)))
	       (if (char-code? ch)
		   (write-char (integer->char ch) out)
		   (begin
		     (display "%" out)
		     (display (number->string ch 16) out)))
	       (for (+ j 1)))))))))

(define (scheme->string s)
  (if (null? s) ""
      (string-append (pct-encode s) ":/")))

(define (authority->string a)
  (if (null? a) ""
      (string-append
       "/"
       (userinfo->string (assoc 'userinfo a))
       (host->string (assoc 'host a))
       (port->string (assoc 'port a)))))

(define (userinfo->string p)
  (if (not p) "" (string-append (pct-encode (cdr p)) "@")))

(define (host->string p)
  (if (not p) "" (pct-encode (cdr p))))

(define (port->string p)
  (if (not p) "" (string-append ":" (pct-encode (cdr p)))))

(define (path->string p)
  (cond
   ((null? p) "")
   ((null? (cdr p)) 
    (string-append "/" (pct-decode (symbol->string (car p)))))
   (else (string-append "/"
			(pct-encode (symbol->string (car p)))
			"/"
			(path->string (cdr p))))))

(define (query->string q)
  (if (and (string? q) (> (string-length q) 0))
      (string-append "?" (pct-encode q))
      ""))

(define (fragment->string f)
  (if (and (string? f) (> (string-length f) 0))
      (string-append "#" f)
      ""))

(define (read-uri #!optional (port (current-input-port)))
  (run (rfc3986) port))

(define (write-uri uri #!optional (port (current-output-port)))
  (print port: port
	 (list
	  (scheme->string (uri-scheme uri))
	  (authority->string (uri-authority uri))
	  (path->string (uri-path uri))
	  (query->string (uri-query uri))
	  (fragment->string (uri-fragment uri))
	  )))

(define (uri #!key 
	     (scheme "http")
	     (authority `())
	     (path '())
	     (query #f)
	     (fragment #f))
  (rfc3986#make-uri scheme authority path query fragment))
