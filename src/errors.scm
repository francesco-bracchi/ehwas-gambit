(##namespace ("ehwas-errors#"))

(##include "~~lib/gambit#.scm")

(include "http-message#.scm")
(include "http-response#.scm")
(include "http-request#.scm")
(include "repr/template#.scm")
(include "errors#.scm")

(##include "~~lib/gambit#.scm")

(declare (standard-bindings)
         (extended-bindings)
         ;;(not safe)
         (block))

(define *default-errors* (make-table test: =))

(define-macro (define-default-for-error kode status)
  (let(
       (code (gensym 'code))
       (data (gensym 'data)))
    `(define-default-error-handler ,kode
       (lambda (,code ,data)
         (http-response
          code:      ,code
          status:    ,status
          header: `((pragma "no-cache")
                    (cache-control . "no-cache, must revalidate")
                    (expires . "-1")
                    (content-type "text/html"))
          
          body:    ,(list 'quasiquote
                          `(top
                            (= "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
                            (html
                             (head (title ,(list 'unquote status) " " ,(list 'unquote code)))
                             (body
                              (center
                               (h1 "Web server error " ,(list 'unquote code))
                               (h5 ,(list 'unquote status))
                               (h4 ,(list 'unquote data))))))))))))

(define-default-for-error 400 "Bad Request")
(define-default-for-error 401 "Unauthorized Access")
(define-default-for-error 403 "Forbidden Access")
(define-default-for-error 404 "File Not Found")
             
(define (string->html s)
  (let(
       (len (string-length s)))
    (call-with-output-string
     (string)
     (lambda (port)
       (let loop ((j 0))
         (if (>= j len) 'ok
             (let(
                  (ch (string-ref s j)))
               (cond
                ((char=? ch #\>) (display "&gt;" port))
                ((char=? ch #\<) (display "&lt;" port))
                ((char=? ch #\&) (display "&amp;" port))
                ((char=? ch #\newline) (display "<br>" port))
                (else
                 (display ch port)))
               (loop (+ j 1)))))))))

(define (exception->string e k)
  (call-with-output-string
   ""
   (lambda (p) (display-exception-in-context e k p))))

(define (backtrace->string k)
  (call-with-output-string
   ""
   (lambda (p)
     (display-continuation-backtrace k p))))

(define-default-error-handler 500
  (lambda (code data)
    (http-response
      code:     500 
      status:  "Internal Server Error"
      header: `((pragma "no-cache")
                (cache-Control "no-cache, must revalidate")
                (expires "-1")
                (content-type "text/html"))
      body:   `(top
                (= "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
                (html
                 (head
                  (title "Webserver Error " 500))
                 (body
                  (center
                   (h1 "Webserver error " 500)
                   (h2 (@ (style .  "align:left")) 
                       (h4 "Internal Server Error")
                       (textarea (@ (cols . "80") (rows . "10") (readonly . "true")) ,data)))))))))


(define (default-handler code data)
  ((table-ref *default-errors* code) code data))

;; (define (with-error-handler handler res)
;;   (lambda (req)
;;     (or (with-exception-catcher
;;          (lambda (e)
;;            (continuation-capture
;;             (lambda (k)
;;               (handler req 500 exception: e continuation: k))))
;;          (lambda () (res req)))
;;         (handler req 404))))

(define (with-error-handler error-handler request-handler)
  (lambda (req)
    (let(
	 (eh (current-exception-handler)))
      (or (with-exception-catcher
	   (lambda (e)
	     (continuation-capture
	      (lambda (kont)
		(with-exception-catcher 
		 (lambda (ex) (pp (unbound-global-exception-variable ex)) 'ignore)
		 (lambda ()
		   (default-handler
                     500
                     (string-append
                      (exception->string e kont)
                      "\n"
                      (backtrace->string kont))))))))
	   (lambda () (request-handler req)))
	  (error-handler 404 "")))))

(define (with-default-error-handler handler)
  (with-error-handler default-handler handler))
