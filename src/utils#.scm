(##namespace 
 ("ehwas-utils#"
  for-each-assoc
  map-assoc
  assh
  string-downcase
  memoize1
  parse-header-fields
  identity
  *html-empty-types*
  let?
  ))

(define-macro (let-if name expr left right)
`(let(
      (,name ,expr))
   (if ,name ,left ,right)))