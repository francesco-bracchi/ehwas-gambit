(##namespace 
 ("ehwas-http-message#"
  
  current-http-reader
  default-http-reader

  current-http-writer
  default-http-writer

  http-reader
  http-writer
  http-reader-set!
  http-writer-set!

  current-http-version

  define-writer
  define-reader

  define-http-message
  http-message-version
  http-message-header
  http-message-body
  http-message?

  http-version-parser
  ))

(define-macro (define-writer head . tail)
  `(http-writer-set! ,(car head) (lambda ,(cdr head) ,@tail)))

(define-macro (define-reader head . tail)
  `(http-reader-set! ,(car head) (lambda ,(cdr head) ,@tail)))

(define-type http-message
  extender: define-http-message
  id: 209121D0-95EA-4061-8C83-5665BBEDAAA3
  (version read-only:)
  (header read-only: unprintable:)
  (body read-only: unprintable:))
