#lang br/quicklang
(require brag/support)
(require parser-tools/lex)

(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(eof)
        eof]
       [":-"
        (token 'IMPLY-TOK lexeme)]
       [(char-set "().:-?")
        lexeme]
       [(from/to "//" "\n")
        (next-token)]
       [(from/to ";" "\n")
        (next-token)]
       [(char-set ",")
        (next-token)]
       [(repetition 1 +inf.0 upper-case)
        (token 'VAR-TOK lexeme )]
       [(repetition 1 +inf.0 lower-case)
        (token 'ATOM-TOK lexeme)]
       [(repetition 1 +inf.0 numeric)
         (token 'NUM-TOK (string->number lexeme))]
       [whitespace
         (next-token)]))
    (our-lexer port))
  next-token)
(provide tokenize)

; (apply-tokenizer tokenize "//Silly example\n ?query(X, Y, 1234).")
