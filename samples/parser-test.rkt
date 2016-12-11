#lang br
(require racketlog/parser
         racketlog/tokenizer
         brag/support)

;(parse-tree (apply-tokenizer tokenize ";Silly example\n fact(1234)."))
;(parse-tree (apply-tokenizer tokenize "//Silly example\n fact(1234).\n ?query(X)."))