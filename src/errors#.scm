(##namespace
 ("ehwas-errors#"
  *default-errors*
  define-default-error-handler
  with-error-handler
  with-default-error-handler))

(define-macro (define-default-error-handler n l)
  `(table-set! *default-errors* ,n ,l))

