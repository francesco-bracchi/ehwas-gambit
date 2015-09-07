;; file: query.scm
;; description:
;; utility that parses a query and returns a table
;; exports:
;; query is a the parser
;; string->query transforms a string in a table
;; author: francesco bracchi (frbracch@gmail.com)
;; transforming query

(namespace ("ehwas-query#"))

(##include "~~lib/gambit#.scm")

(include "utils#.scm")
(include "http-message#.scm")
(include "rfc822#.scm")
(include "rfc3986#.scm")
(include "repr/rfc2388#.scm")
(include "repr/urlencoded#.scm")
(include "http-request#.scm")

(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/sources/port#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (not safe)
	 (not inline)
         (fixnum))

(define (really-parse-query str)
  (call-with-input-string
   str
   (lambda (port)
     (run (urlencoded) port))))

(define parse-query (memoize1 really-parse-query))

(define (get-query request)
  (let-if url-query (uri-query (http-request-uri request))
	  (parse-query url-query)
	  '()))

(define (post-query request)
  (let((body (http-message-body request)))
    (if (pair? body) body '())))

(define (request-query request)
  (append (get-query request)
	  (post-query request)))

(define (with-query request fields thunk)
  (let((query (request-query request)))
    (apply thunk (map (lambda (j) (assh j query)) fields))))

