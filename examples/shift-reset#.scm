(##namespace 
 ("shift-reset#"
  call/reset
  call/shift
  reset
  shift))

(define-macro (reset . body)
  `(call/reset (lambda () ,@body)))

(define-macro (shift kont . body)
  `(call/shift (lambda (,kont) ,@body)))
