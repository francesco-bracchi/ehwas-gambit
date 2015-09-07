(namespace ("ehwas-rfc2388#"))

(##include "~~lib/gambit#.scm")
(include "~~ansuz/char-stream-parser#.scm")
(include "~~ansuz/re#.scm")
(include "~~ansuz/sources/string#.scm")

(include "../utils#.scm")
(include "../http-message#.scm")
(include "../rfc822#.scm")
;;(include "query#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (block)
	 (not inline)
	 (not safe)
	 (fixnum)
         )

(define max-file-size (make-parameter (* 1024 1024))) ;; default 1Mb

(define-regexp spaces " *")

(define-parser (field)
  (<- name (regexp "[~= \n]*"))
  (spaces) 
  (maybe #\=)
  (spaces)
  (maybe #\")
  (<- value (regexp "[~;\n\"]*"))
  (maybe  #\")
  (ret (cons (string->symbol name) value)))

(include "~~ansuz/sources/port#.scm")

(define (stream-starts? stream boundary)
  (let starts? ((stream stream)
		(index 0))
    (cond
     ((>= index (string-length boundary)) stream)
     ((eq? (stream-car stream) (string-ref boundary index))
      (starts? (stream-cdr stream) (+ index 1)))
     (else #f))))

(define (fold f i l)
  (let fold ((i i) (l l))
    (if (null? l) i
	(fold (f i (car l)) (cdr l)))))

(define (append-u8vector vectors total-length)
  (let*(
	(result (make-u8vector total-length))
	(fill (lambda (position vector)
		(let*(
		      (vector-len (u8vector-length vector))
		      (available (- total-length position))
		      (rooms (min vector-len available)))
		  (subu8vector-move! vector 0 rooms result position)
		  (+ position rooms)))))
    (fold fill 0 vectors)
    result))

(define (u8vector->string u8-vect)
  (call-with-input-u8vector 
   u8-vect
   (lambda (port) (read-line port #f))))

(define-parser (rfc2388-value boundary)
  (reflect 
   (stream success fail)
   (let(
	(boundary (string-append "\n--" boundary )))
     (let get-value ((stream stream)
		     (current (make-u8vector 64))
		     (current-index 0)
		     (others '())
		     (total-length 0))
       (let(
	    (ch (stream-car stream)))
	 (cond
	  ((>= total-length (max-file-size))
	   (fail "length exceed max file length"))
	  ((>= current-index (u8vector-length current))
	   (get-value stream
		      (make-u8vector (* 2 (u8vector-length current)))
		      0
		      (cons current others)
		      total-length))
	  
	  ((eof-object? ch)
	   (fail "end of object reached before boundary" stream ch))
	  
	  ((stream-starts? stream boundary) => 
	   (lambda (stream0)
	     (success (append-u8vector (reverse (cons current others))
				       total-length)
		      stream0
		      fail)))
	  (else 
	   (u8vector-set! current current-index (char->integer ch))
	   (get-value (stream-cdr stream)
		      current
		      (+ current-index 1)
		      others
		      (+ 1 total-length)))))))))

(define-parser (rfc2388-field boundary)
  (<- header* (rfc822))
  (let* ((header (parse-header-fields header*))
	 (content-disposition_ (assq 'content-disposition header))
	 (content-disposition (cadr content-disposition_))
	 (content-type_ (assq 'content-type header))
	 (content-type (and content-type_ (cdr content-type_)))
	 (name (assh 'name content-disposition))
	 (filename (assh 'filename content-disposition)))
    (if (or (not name) (= (string-length name) 0)) (fail "name not specified")
	(cat (<- value (rfc2388-value boundary))
	    (ret (cons (string->symbol name)
			  (if filename
			      (list (call-with-input-u8vector 
				     value
				     (lambda (port)
				       ((current-http-reader) header port)))
				    (cons 'filename filename)
				    (cons 'content-type content-type))
			      (u8vector->string value))))))))

(define-parser (rfc2388)
  #\- #\-
  (<- boundary (regexp "[~\n]+"))
  #\newline
  (<- first (rfc2388-field boundary))
  (<- rest (many (cat #\newline (rfc2388-field boundary))))
  (ret (cons first rest)))

(define (read-rfc2388 header port)
  (run (rfc2388) port))

(define current-boundary (make-parameter #f))

(define (make-boundary) 
  (call-with-output-string
   ""
   (lambda (port)
     (print port: port
	    (map (lambda (i) (number->string i 16))
		 (map random-integer 
		      (vector->list (make-vector 32 16))))))))


(define (write-data data port)
  (cond
   ((string? data)(display data port))
   ((u8vector? data)(write-subu8vector data 0 (u8vector-length data) port))
   (else 
    (error "I don't know how to encode data, should be string or u8vectors"))))


(define (write-string-data key value boundary #!optional (port (current-output-port)))
  (print port: port (list "--" boundary "\n"))
  (write-rfc822
   `((Content-disposition form-data ,(string-append "name=\"" key "\"")))
   port)
  (newline port)
  (display value port)
  (newline port))

(define (write-file-data key value boundary #!optional (port (current-output-port)))
  (let(
       (data (car value))
       (filename (assq 'filename (cdr value)))
       (content-type (assq 'content-type (cdr value))))
    (print port: port (list "--" boundary "\n"))
    (write-rfc822
     `((content-disposition form-data 
			    ,(list "name=\"" key "\"")
			    ,(list "filename=\"" (cdr filename) "\""))
       (content-type ,(or (and content-type (cdr content-type)) "")))
     port)
    (newline port)
    ((current-http-writer) data (cdr value) port)
    (newline port)))

(define (get-header-boundary header)
  (let*(
	(type (assq 'content-type header))
	(type (and type (cadr type))))
    (and (pair? type) (assh 'boundary (cdr type)))))

(define (write-rfc2388 data header port)
  (if (u8vector? data)
      (write-subu8vector data 0 (u8vector-length data) port)
      (let((boundary (or (get-header-boundary header)
			 (make-boundary))))
	(for-each-assoc 
	 (lambda (key value)
	   (if (and (pair? value) (assq 'filename (cdr value)))
	       (write-file-data key value boundary port)
	       (write-string-data key value boundary port)))
	 data)
	(print port: port (list "--" boundary "--")))))

(http-reader-set! "multipart/form-data" read-rfc2388)
(http-writer-set! "multipart/form-data" write-rfc2388)
