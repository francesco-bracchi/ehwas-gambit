(##namespace ("ehwas-json#"))

(##include "~~lib/gambit#.scm")

(include "~~ansuz/sources/port#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")

(include "../utils#.scm")
(include "../http-message#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
	 (not inline)
         ;; (not safe)
         (fixnum))

(define-regexp space "[\t\n ]*")

(define-parser (json)
  (alt (json-object)
       (json-array)
       (json-true)
       (json-false)
       (json-null)
       (json-string)
       (json-number)))

(define-parser (json-object)
  #\{
  (space)
  (<- frst (json-object-pair))
  (<- rest (many (cat (space) #\, (space) (json-object-pair))))
  (space)
  #\}
  (ret (cons frst rest)))

(define-parser (json-object-pair)
  (<- key (json-string))
  (space)
  #\:
  (space)
  (<- x (get-stream))
  (<- value (json))
  (<- y (get-stream))
  (ret (cons (string->symbol key) value)))

(define-parser (json-array)
  #\[
  (space)
  (<- first (json))
  (<- rest (many (cat (space) #\, (space) (json))))
  (space)
  #\]
  (ret (list->vector (cons first rest))))

(define-parser (json-true)
  (regexp "true")
  (ret #t))

(define-parser (json-false)
  (regexp "false")
  (ret #f))

;; it is void because '() is the empty dictionary
(define-parser (json-null)
  (regexp "null")
  (ret #!void))

(define-parser (json-string)
  #\"
  (<- string (many (alt (cat #\\ (any)) (get-if (lambda (ch) (not (eq? ch #\")))))))
  ;; (<- string (regexp "([~\"]|\\\\\")*"))
  #\"
  (ret (list->string  string)))

(define-parser (json-number)
  (<- num (regexp "\\-?(0|[1-9][0-9]*)(\\.[0-9]*)?([eE][\\+\\-]?[0-9]*)?"))
  (ret (string->number num)))

(define (read-json header port)
  (run (json) port))

(define (write-json-pair pair port)
  (write (symbol->string (car pair)) port)
  (display #\:)
  (write-json-generic (cdr pair) port))

(define (write-json-object object port)
  (display "{" port)
  (let loop ((pairs object))
    (if (pair? pairs)
	(begin
	  (write-json-pair (car pairs) port)
	  (if (pair? (cdr pairs)) (display #\, port))
	  (loop (cdr pairs)))))
  (display "}" port))

(define (write-json-array array port)
  (display "[" port)
  (let loop ((j 0))
    (if (< j (vector-length array))
	(begin
	  (write-json-generic (vector-ref array j) port)
	  (if (< (+ j 1) (vector-length array)) (display #\, port))
	  (loop (+ j 1)))))
  (display "]" port))

(define (write-json-string string port)
  (write string port))

(define (write-json-number number port)
  (write number port))

(define (write-json-boolean bool port)
  (display (if bool "true" "false") port))

(define (write-json-null port)
  (display "null" port))

(define (write-json-generic object port)
  (cond
   ((list? object) (write-json-object object port))
   ((vector? object) (write-json-array object port))
   ((string? object) (write-json-string object port))
   ((number? object) (write-json-number object port))
   ((boolean? object) (write-json-boolean object port))
   ((eq? object #!void) (write-json-null port))
   (else (error "wrong encoding format"))))

(define (write-json object header port)
  (cond
   ((u8vector? object) (write-subu8vector object 0 (u8vector-length object) port))
   ((procedure? object) (object port))
   (else (write-json-generic object port))))

(http-reader-set! "application/json" read-json)
(http-writer-set! "application/json" write-json)

;;mapping
;; array <-> vector
;; object <-> alist `((<string> value) ...)
;; null <-> #!void 
;; number <-> number
;; boolean <-> boolean
