;; exported values of query.scm
;; author: francesco bracchi (frbracch@gmail.com)

(##namespace
 ("ehwas-query#"
  get-query
  post-query
  request-query
  with-query
  with-fields
))

(define-macro (with-fields request fields . body)
  `(with-query ,request ',fields (lambda ,fields ,@body)))
