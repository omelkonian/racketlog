#lang br/quicklang
(require racketlog)
(require racketlog/inference)

(provide ; Transliteration
         racketlog-program clause query functor
         ; REPL
         ;#%top-interaction
         ; Macro Setup
         (rename-out [racketlog-module-begin #%module-begin]))

(define-macro (racketlog-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE     
     ))

; Transliteration
(define-macro (racketlog-program CLAUSES ... QUERY)  
  #'(begin
      (displayln "============= KNOWLEDGE BASE ============")
      ; Populate knowledge base (KB)
      (define kb (list CLAUSES ...))
      ; Print KB
      (map (lambda (t)
             (println t))
           kb)
      (displayln "==========================================")
      ; Print query
      (displayln (list "?" (list QUERY)))
     ))

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
