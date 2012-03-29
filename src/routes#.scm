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

(include "../utils/match#.scm")

(define-macro (handler-case r . patterns)
  (let((m (gensym 'method))
       (u (gensym 'uri))
       (p (gensym 'path)))
    `(let*((,m (http-request-method ,r))
           (,u (http-request-uri ,r))
           (,p (uri-path ,u)))
       (match (cons m p) ,patterns))))
              
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
