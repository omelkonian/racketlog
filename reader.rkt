#lang br/quicklang
(require "tokenizer.rkt" "parser.rkt")

; Reader
(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module racketlog-mod racketlog/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)