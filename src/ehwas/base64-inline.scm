(##namespace ("ehwas-base64-inline#"
	      base64-inline))

(include "../encode/base64#.scm")
(include "mime-types#.scm")

(define (base64-inline fn)
  (if (and (file-exists? fn)
           (eq? (file-type fn) 'regular))
      (call-with-input-file fn
        (lambda (in-port)
          (let(
               (data (make-u8vector (file-size fn))))
            (read-subu8vector data 0 (u8vector-length data) in-port)
            (string-append "data:" (mime-type fn) ";base64," (u8vector->base64-string data)))))))