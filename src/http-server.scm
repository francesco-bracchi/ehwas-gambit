(##namespace ("ehwas-http-server#"))

(##include "~~/lib/gambit#.scm")

(include "utils#.scm")
(include "http-message#.scm")
(include "http-request#.scm")
(include "http-response#.scm")
;; (include "repr/template#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block))

;; (define current-http-port (make-parameter #f))
(define max-idle-time (make-parameter 30))

(define response-404
  (http-response
   code: 404
   status: "Not Found"
   header: `((content-type . "text/html"))
   body:  `(top 
	    (html 
	     (head (title "Error 404"))
	     (body 
	      (h1 "Error 404")
	      (p "File not found"))))))

(define response-400
  (http-response
   code: 400
   status: "Bad Request"
   header: `((content-type . "text/html"))
   body: `(top 
	    (html 
	     (head (title "Error 400"))
	     (body 
	      (h1 "Error 400")
	      (p "Bad Request"))))))

(define (response-500 cont exc)
  (let((err
	(call-with-output-string 
	 ""
	 (lambda (port)
	   (display-exception-in-context exc cont port)
	   (display-continuation-backtrace cont port #t #t 10 4)))))
    (http-response
     code: 500
     status: "Internal Server Error"
     header: `((content-type . "text/html"))
     body: `(top 
	     (html 
	      (head (title "Error 500"))
	      (body 
	       (h1 "Error 500")
	       (p "Internal Server Error")
	       (pre ,err)))))))

(define (http-server-exception-handler ex)
  (write-http-response (response-500 ex))
  (force-output))

(define (http-service handler)
  (lambda () 
    (input-port-timeout-set! (current-input-port) (max-idle-time))
    (call/cc
     (lambda (exit)
       (let handle-more ()
	 (with-exception-handler
	  (lambda (ex)
	    (continuation-capture
	     (lambda (cont)
	       (display-exception-in-context ex cont (current-output-port))
	       (display-continuation-backtrace cont (current-output-port) #t #t 10 4)))
	    (exit #f))
	  (lambda ()
	    (let((request (read-http-request)))
	      (if (http-request? request)
		  (let ((response (handler request)))
		    (if (http-response? response)
			(let*((request-header (http-message-header request))
			      (response-header (http-message-header response))
			      (request-connection  (assh 'connection request-header))
			      (response-connection (assh 'connection response-header))
			      (keep-alive? (and (equal? (http-message-version request) '(1 . 1))
						request-connection 
						(equal? (car request-connection)  "Keep-Alive")
						(not (and response-connection 
							  (equal? (car response-connection) "Close"))))))
			  (write-http-response response)
			  (force-output)
			  (if keep-alive? (handle-more)))
			(write-http-response response-404)))
		  (write-http-response response-400))))))))))

;; (define (with-tcp-port handler)
;;   (lambda ()
;;     (parameterize
;;      ((current-http-port (current-input-port)))
;;      (handler))))

(define (http-service-register! 
	 handler 
	 #!key
	 (server-address "*")
	 (port-number 80)
	 (backlog 128) 
	 (reuse-address #t))

  (tcp-service-register!
   (list server-address: server-address
         port-number: port-number
         backlog: backlog
         reuse-address: reuse-address
         char-encoding: 'ASCII
         eol-encoding: 'cr-lf
         buffering: #f)
   (http-service handler)))
