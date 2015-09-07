(##namespace("ehwas-mime-types#"))
(##include "~~lib/gambit#.scm")

(declare (standard-bindings)
         (extended-bindings)
         (fixnum)
         ;; (not safe)
         (block))

(define types (make-table test: string-ci=? init: "text/plain"))

(define (mime-type-set! ext mime)
  (table-set! types ext mime))

(mime-type-set!  ".js"    "text/javascript")
(mime-type-set!  ".json"  "application/json")
(mime-type-set!  ".css"   "text/css")
(mime-type-set!  ".html"  "text/html")
(mime-type-set!  ".htm"   "text/html")
(mime-type-set!  ".shtml" "text/html")
(mime-type-set!  ".html"  "text/html")
(mime-type-set!  ".txt"   "text/plain")
(mime-type-set!  ".xhtml" "application/xhtml+xml")
(mime-type-set!  ".xht"   "application/xhtml+xml")
(mime-type-set!  ".svg"   "image/svg+xml")
(mime-type-set!  ".svgz"  "image/svg+xml")
(mime-type-set!  ".png"   "image/png")
(mime-type-set!  ".gif"   "image/gif")
(mime-type-set!  ".jpg"   "image/jpeg")
(mime-type-set!  ".jpeg"  "image/jpeg")
(mime-type-set!  ".jpe"   "image/jpeg")
(mime-type-set!  ".xml"   "application/xml")
(mime-type-set!  ".zip"   "application/zip")

(mime-type-set!  ".py"    "text/x-python")
(mime-type-set!  ".pl"    "text/x-perl")
(mime-type-set!  ".pm"    "text/x-perl")
(mime-type-set!  ".lisp"  "text/x-lisp")
(mime-type-set!  ".scm"   "text/x-scheme")
(mime-type-set!  ".rb"    "text/x-ruby")
(mime-type-set!  ".php"   "text/x-php")

(mime-type-set!  ".hs"    "text/x-haskell")
(mime-type-set!  ".lhs"    "text/x-literate-haskell")

(mime-type-set!  ".h"     "text/x-chdr")
(mime-type-set!  ".c"     "text/x-csrc")

(mime-type-set!  ".c++"     "text/x-c++src")
(mime-type-set!  ".cpp"     "text/x-c++src")
(mime-type-set!  ".cxx"     "text/x-c++src")
(mime-type-set!  ".cc"      "text/x-c++src")

(mime-type-set!  ".h++"     "text/x-c++hdr")
(mime-type-set!  ".hpp"     "text/x-c++hdr")
(mime-type-set!  ".hxx"     "text/x-c++hdr")
(mime-type-set!  ".hh"      "text/x-c++hdr")

(mime-type-set!  ".java"   "text/x-java")

(mime-type-set!  ".sh"     "text/x-sh")

(mime-type-set!  ".tcl"    "text/x-tcl")
(mime-type-set!  ".tk"     "text/x-tcl")

(mime-type-set!  ".tex"    "text/x-tex")
(mime-type-set!  ".ltx"    "text/x-tex")
(mime-type-set!  ".sty"    "text/x-tex")
(mime-type-set!  ".cls"    "text/x-tex")

(mime-type-set! ".ico"    "image/x-icon")

(define (mime-type fn)
  (table-ref types (path-extension fn)))

