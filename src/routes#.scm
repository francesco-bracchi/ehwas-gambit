;;; EXAMPLE

;; (define (handler req)
;;   (case-handler req
;;    (('GET 'foo 'bar) ;; easy static
;;     (response body: "foobar"))

;;    ((_ 'foo 'bar) ;; ignore method
;;     (response body: "method ignored"))

;;    ((method 'foo 'bat) ;;bound method
;;     (response body: (sring-append "method bound: " (symbol->string method))))

;;    (('GET 'foo bar) ;; last segment can be any, matched by bar
;;     (response body: (string-append "foo" (do-something bar))))

;;    (('GET 'foo . rest) ;; tail of path bound to rest 
;;     (response body: (string-append "doo" (do-something-with-list rest))))

;;    (('POST 'foo 'bar) ;; use of post
;;     (response body:  "posted to foo/bar"))

;;    (('PUT 'foo 'bar yy mm dd 'other v)
;;     (response body: "rather complex example"))

;;    ((method . rest) ;; match anything
;;     (response body: "anything")))

;; it can be replaced with
;; (define-handler (handler req)
;;   (('GET 'foo 'bar) ...)
;    ((_ 'foo 'bar) ...))

(##namespace ("ehwas-routes#" 
              handler-case
              define-handler
              ))

(define-macro (ehwas-routes#match value . patterns)
  (define (quoted? x)
    (and (pair? x) (eq? (car x) 'quote)))

  (define (const? x)
    (or (quoted? x)
	(number? x)
	(char? x)))

  (define (match-pair value pattern action fail)
    (let((v (gensym 'value))
	 (a (gensym 'car))
	 (d (gensym 'cdr))
	 (f (gensym 'fail)))
      `(let((,v ,value))
	 (if (pair? ,v)
	     (let((,a (car ,v))
		  (,d (cdr ,v))
		  (,f (lambda () ,fail)))
	       ,(match-pattern a 
			       (car pattern) 
			       (match-pattern d (cdr pattern) action `(,f))
			       `(,f)))))))

  (define (match-vector value pattern action fail)
    (let((v (gensym 'value))
	 (f (gensym 'fail)))
      `(let((,v ,value)
	    (,f (lambda () ,fail)))
	 (if (and (vector? ,v) (= (vector-length ,v) ,(vector-length pattern)))
	     ,(let matcher ((as (list->vector pattern)) (j 0))
		(if (null? as) `(,f)
		    (match-pattern `(vector-ref ,v ,j) 
				   (car as) 
				   (matcher (cdr as) (+ j 1))
				   `(,f))))))))))

(define (match-pattern value pattern action fail)
  (cond
   ((null? pattern)`(if (null? ,value) ,action ,fail))
   ((eq? pattern '_) action)
   ((const? pattern) `(if (eq? ,value ,pattern) ,action ,fail))
   ((symbol? pattern) `(let((,pattern ,value)) ,action))
   ((pair? pattern) (match-pair value pattern action fail))
   ((vector? pattern) (match-vector valeu pattern action fail))
   (else (error "pattern matching failed at " ,pattern))))

  (cond
   ((null? patterns) 
    `(error "cannot match"))
   ((eq? (caar patterns) 'else)
    `(begin ,@(cdar patterns)))
   (else 
    (let((pattern (caar patterns))
         (action `(begin ,@(cdar patterns)))
         (v (gensym 'value)))
      `(let((,v ,value))
         ,(match-pattern v pattern action
                         `(ehwas-routes#match ,v ,@(cdr patterns))))))))


(define-macro (handler-case r . patterns)
  (let((m (gensym 'method))
       (u (gensym 'uri))
       (p (gensym 'path)))
    `(let*((,m (http-request-method ,r))
           (,u (http-request-uri ,r))
           (,p (uri-path ,u)))
       (ehwas-routes#match (cons m p) ,patterns))))
              
(define-macro (define-handler head . patterns)
  (let((request-name (cadr head)))
    `(define ,head 
       (handler-case ,request-name ,@patterns))))

;; (define-handler (public-routes req)
;;   (('GET 'issues id) 
;;    (with-fields req (key) (get-issue id key)))
;;   (('GET 'issues) 
;;    (with-fields req (limit offset) (list-issues id key))))

;; (define (public-routes req)
;;   (handler-case
;;    req 
;;    (('GET 'issues)
;;     (with-fields req (limit offset) (list-issues key limit offset)))
;;    (('GET 'issues id)
;;     (with-fields req (key) (list-issues key limit offset)))))
