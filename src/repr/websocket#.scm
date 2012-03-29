(##namespace
 ("ehwas-websocket#"
  websocket
  
  websocket-reader
  websocket-writer
  
  websocket-read
  websocket-write
  
  websocket-protocol-set!
  definf-websocket-protocol))

(define-macro (define-websocket-protocol name read write)
  `(websocket-protocol-set! ,(if (symbol? name) (symbol->string name) name)
                            ,read
                            ,write))


                