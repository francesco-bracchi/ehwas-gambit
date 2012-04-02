(define current-websocket-encoder (make-parameter display))

(define current-websocket-decoder (make-parameter read))

(define (websocket-read #!optional
			(decoder (current-websocket-decoder))
			(port (current-input-port)))
  (let*(
	(data (decoder port))
	(endf (read-u8 port)))
    (if (not (or (eof-object? endf) (= endf #x00)))
	(error "websocket should end with #x00 byte")
	data)))

(define (websocket-write data 
			 #!optional
			 (encoder (current-websocket-encoder))
			 (port (current-output-port)))
  (write-u8 #x00 port)
  (encoder data port)
  (write-u8 #xff port))


(define (websocket handler)
  (lambda (request)
    (
