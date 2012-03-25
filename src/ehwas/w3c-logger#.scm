(##namespace
 ("ehwas-w3c-logger#"
  make-w3clogger
  ;; with-log
  log!
  ))

;; (define-macro (with-log fn res)
;;   (let(
;;        (log (gensym 'log)))
;;     `(let(
;;           (,log (open-output-file (list path: ,fn append: #t))))
;;        (make-w3clogger ,res ,log))))

