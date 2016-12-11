#lang racket
(provide unify variable? constant?)

; Helpers
(define (variable? s)
  (and (string? s)
       (char-upper-case? (string-ref s 0))))
                   
(define (constant? s)
  (or (equal? s '())
      (and (string? s)
       (char-lower-case? (string-ref s 0)))))

; Occurs check
(define (occurs? variable pattern)
  (cond
    [(equal? variable pattern) #t]
    [(or (variable? pattern) (constant? pattern)) #f]
    [(or (occurs? variable (car pattern))
         (occurs? variable (cdr pattern)))]))

; Bindings
(define (add-binding variable value bindings)
  (cons (cons variable value) bindings))

; Unification
(define (match-variable variable pattern bindings)
  (cond
    [(equal? variable pattern)
     bindings]
    [(let ([binding (assoc variable bindings)])
       (cond
         [binding
          (unify (cdr binding) pattern bindings)]
         [(occurs? variable pattern)
          'fail]
         [(add-binding variable pattern bindings)]))]))

(define (unify pattern-1 pattern-2 bindings)
  ;(displayln (format "\tUNIFY ~a ?? ~a" pattern-1 pattern-2))
  ;(displayln (format "\t\t{~a}" bindings))
  (cond
    [(equal? bindings 'fail) 'fail]
    [(variable? pattern-1)
     (match-variable pattern-1 pattern-2 bindings)]
    [(variable? pattern-2)
     (match-variable pattern-2 pattern-1 bindings)]
    [(constant? pattern-1)
     (if (equal? pattern-1 pattern-2)
         bindings
         'fail)]    
    [(constant? pattern-2)
     'fail]
    [(unify (cdr pattern-1)
            (cdr pattern-2)
            (unify (car pattern-1)
                   (car pattern-2)
                   bindings))]))


; TEST

;(unify (cons "p" "X") (cons "p" "orestis") '())
;(unify '("p" "X" "Y") '("p" "x" "y") '())
;(unify '("father" "X" "o") '("father" "p" "o") '())
;(unify '("father" "X" "o") '() '())
