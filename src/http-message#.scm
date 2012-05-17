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
  `(reader-set! ,(car head) (lambda ,(cdr head) ,@tail)))

(define-macro (define-reader head . tail)
  `(reader-set! ,(car head) (lambda ,(cdr head) ,@tail)))