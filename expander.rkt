#lang br/quicklang
(require racketlog)
(require racketlog/inference)

(provide ; Transliteration
         racketlog-program clause query functor
         ;#%top-interaction
         ; Macro Setup
         (rename-out [racketlog-module-begin #%module-begin]))

(define-macro (racketlog-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))

; REPL
;(define-macro (top-interaction . form)
 ; (displayln #'form))

; Transliteration
(define-macro (racketlog-program CLAUSES ... QUERY)
  #'(begin
      (define KB (list CLAUSES ...))      
      (run QUERY KB)
      (displayln "No more solutions.")))

(define-macro-cases clause
  [(clause (predicate PRED) ".")
   #'(list PRED)]
  [(clause (predicate HEAD) ":-" (predicate-list (predicate PRED) ...) ".")
   #'(list HEAD PRED ...)])

(define-macro (query "?" (predicate-list (predicate PRED) ...) ".")
  #'(list PRED ...))

(define-macro (functor NAME "(" (term-list (term TERM) ...) ")")
  ;(with-pattern ([S-NAME (format-datum '~a #'NAME)])
  #'(list 'NAME 'TERM ...))
