(##namespace 
 ("ehwas-template#"
  template->function
  run
  simplify-html
  simplify-xml
  html
  xml
  define-html
  define-xml
  ))

;;; FIND SOMETHING LIKE 
;; (define-template (foo a b c #!key d e f #!optional g #!rest h)
;;   (html
;;    (head (title ($ a)))
;;    ...))

(define-macro (html . body)
  `(template->function
    (simplify-html 
     ,(list 'quasiquote `(top ,@body)))))

(define-macro (xml . body)
  `(template->function
    (simplify-xml
     ,(list 'quasiquote `(top ,@body)))))

(define-macro (define-html name . body)
  `(define ,name (html ,@body)))

(define-macro (define-xml name . body)
  `(define ,name (xml ,@body)))

 