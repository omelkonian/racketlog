#lang brag

racketlog-program : clause* query

clause            : predicate "."
                  | predicate IMPLY-TOK predicate-list "."

predicate         : ATOM-TOK
                  | functor

predicate-list    : predicate+

query             : "?" predicate-list "."

functor           : ATOM-TOK "(" term-list ")"

term-list         : term+

term              : NUM-TOK | ATOM-TOK | VAR-TOK | functor
