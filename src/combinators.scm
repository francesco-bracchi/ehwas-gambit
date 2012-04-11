(##namespace ("ehwas-combinators#"))

(##include "~~/lib/gambit#.scm")

(include "http-message#.scm")
(include "http-response#.scm")
(include "http-request#.scm")
(include "rfc3986#.scm")
(include "errors#.scm")

(include "query#.scm")
(include "cookies#.scm")
(include "mime-types#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;; (not safe)
         (block))

(define (orelse . rs)
  (lambda (request)
    (let test ((rs rs))
      (and (not (null? rs))
           (or ((car rs) request)
               (test (cdr rs)))))))


(define (cache handler #!key (getter http-request-uri))
  (let(
       (*cache* (make-table weak-keys: #t test: equal?)))
    (lambda (req)
      (let(
           (key (getter req)))
        (or (table-ref *cache* key #f)
            (let(
                 (res (handler req)))
              (if (and (http-response? res) (eq? (http-response-code res) 200))
                  (table-set! *cache* key res))
              res))))))

(define (regular-file? f)
  (and (file-exists? f)
       (eq? (file-type f) 'regular)))

(define (filesystem root #!key (buffer-size 16384))
  (lambda (request)
    (let*(
	  (local-path (map (lambda (t) (string-append "/" (symbol->string t))) 
			   (uri-path (http-request-uri request))))
	  (path (apply string-append (cons root local-path))))
      (and (file-exists? path)
	   (eq? (file-type path) 'regular)
	   (let(
		(buffer (make-u8vector buffer-size))
		(size (file-size path))
		(etag (number->string
		       (time->seconds
			(file-last-modification-time path))))
		(if-none-match (assq 'if-none-match (http-message-header request))))
	     (if (and if-none-match (equal? etag (cdr if-none-match)))
		 (http-response code: 304 status: "Not Modified")
		 (http-response
		  header: `((content-type . ,(mime-type path))
			    (content-length . ,size)
			    (etag . ,etag))
		  body:
		  (lambda (out-port)
		    (call-with-input-file path
		      (lambda (port)
			(let buffer-write ((j 0))
			  (if (< j size)
			      (let(
				   (delta (read-subu8vector buffer 0 buffer-size port)))
				(write-subu8vector buffer 0 delta out-port)
				(force-output out-port)
				(buffer-write (+ j delta)))))))))))))))

(define (with-table table #!key (getter 
				 (lambda (request)
				   (cons (http-request-method request)
					 (uri-path (http-request-uri request))))))
  (lambda (request)
    ((table-ref table (getter request) (lambda (request) #f)) request)))

(define (request-with-new-path request path)
  (make-http-request
   (http-message-version request)
   (http-message-header request)
   (http-message-body request)
   (http-request-method request)
   (let(
        (uri (http-request-uri request)))
     (make-uri
      (uri-scheme uri)
      (uri-authority uri)
      path
      (uri-query uri)
      (uri-fragment uri)))))

(define (with-index file handler)
  (lambda (request)
    (let(
         (reverse-path (reverse (uri-path (http-request-uri request)))))
      (if (and (pair? reverse-path)
	       (eq? (car reverse-path) '||))
	  (handler
	   (request-with-new-path request (reverse (cons file (cdr reverse-path)))))
	  (handler request)))))

(define (strip-path-prefix path prefix)
  (cond
   ((null? prefix) path)
   ((null? path) #f)
   ((eq? (car prefix) (car path)) (strip-path-prefix (cdr path) (cdr prefix)))
   (else #f)))

(define current-prefix (make-parameter '()))

(define (local-ref l #!optional (prefix (current-prefix)))
  (append prefix l))

(define (with-prefix prefix handler)
  (lambda (request)
    (let((path1 (strip-path-prefix (uri-path (http-request-uri request)) prefix)))
      (and path1
	   (parameterize
	    ((current-prefix (append (current-prefix) prefix)))
	    (handler (request-with-new-path request path1)))))))

(define (allow test? handler)
  (lambda (request)
    (and (test? request) (handler request))))

(define (deny test? handler)
  (lambda (request)
    (if (test? request) #f (handler request))))

(define (get? request)
  (eq? (http-request-method request) 'GET))

(define (post? request)
  (eq? (http-request-method request) 'POST))

(define (head? request)
  (eq? (http-request-method request) 'HEAD))

(define (options? request)
  (eq? (http-request-method request) 'OPTIONS))

(define (implode es)
  (if (null? es) ""
      (apply string-append
             (cons (car es) (map (lambda (e) (string-append "/" e)) (cdr es))))))

(define (redir to)
  (http-response
   code: 302 
   status: "Found"
   header: `((location ,(if (list? to) (implode (map symbol->string to)) to))
             (content-type "text/plain"))
   body: ""))

(define (redirect to)
  (lambda (req) (redir to)))
   
(define (catch-exception exception-handler handler)
  (lambda (request)
    (with-exception-catcher
     (lambda (ex) (exception-handler request ex))
     (lambda () (handler request)))))

(define (no-cache handler)
  (lambda (request)
    (let(
	 (response (handler request)))
      (and response
	   (make-http-response
	    (http-message-version response)
	    (append
	     '((pragma "no-cache")
	       (cache-control "no-cache" "must revalidate")
	       (expires -1))
	     (http-message-header response)
	     (http-message-body response)
	     (http-response-code response)
	     (http-response-status response)))))))

(define (compose f . fs)
  (let compose ((f f) (fs fs))
    (if (null? fs) f
	(let((g (car fs)))
	  (compose (lambda (x) (f (g x))) (cdr fs))))))
