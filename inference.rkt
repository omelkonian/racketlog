#lang racket
(require racket/stream)
(require racketlog/unification)
(provide infer)

; KB
(define assertions
  '(
    (("father" "pavlos" "orestis"))
    (("mother" "alkisti" "orestis"))
    (("couple" "X" "Y") ("father" "X" "Z") ("mother" "Y" "Z"))
    ))

; Helpers
(define conclusion car)

(define premise cdr)

(define (list-of-lists? list)
  (and (list? list)
       ((length list) . equal? . 1)
       (list? (car list))))

(define (single->nested single)
  (cond [(list-of-lists? single) single]
        [(list? single) (list single)]
        [else (error 'NEST-ERROR)]))

(define (rule? clause)
  ;(displayln (format "Rule? ~a : ~a" clause (and (list? clause) ((length clause) . > . 1))))
  (and (list? clause)
       ((length clause) . > . 1)))

(define (conjunctive? goal)
  ;(displayln (format "Conjunctive? ~a : ~a" goal (and (list? goal) (list? (car goal)) ((length goal) . > . 1))))
  (and (list? goal)
       (list? (car goal))
       ((length goal) . > . 1)))

; Renaming
(define (rename-variables assertion)
  (define variables (extract-variables assertion))
  (define replace-map (generate-ids variables))
  (rename assertion replace-map))

(define (extract-variables expr)
  (cond [(constant? expr) '()]
        [(variable? expr) (list expr)]
        [(append (extract-variables (car expr))
                 (extract-variables (cdr expr)))]))

(define (generate-ids variables)
  (map (lambda (var) (cons var (gen-var))) variables))

(define (gen-var)
  (string-titlecase (symbol->string (gensym))))

(define (rename expr replace-map)
  (cond [(constant? expr) expr]
        [(variable? expr) (cdr (assoc expr replace-map))]
        [(cons (rename (car expr) replace-map)
               (rename (cdr expr) replace-map))]))

; Inference
(define (solve goal bindings)
  ;(displayln (format "SOLVE ~a" goal))
  ;(displayln (format "\t{~a}" bindings))
  (if (conjunctive? goal)
      (filter-goals goal (stream bindings))
      (infer goal bindings assertions)))

(define (filter-goals goals bindings-stream)
  ;(displayln (format "FILTERS ~a" goals))
  (if (null? goals)
      bindings-stream
      (filter-goals (cdr goals)
                    (filter-goal (single->nested (car goals))
                                 bindings-stream))))

(define (filter-goal goal bindings-stream)
  ;(displayln (format "FILTER ~a" goal))
  (if (stream-empty? bindings-stream)
      empty-stream
      (stream-append (solve goal (stream-first bindings-stream))
                     (filter-goal goal
                                  (stream-rest bindings-stream)))))

(define (infer goal bindings kb)
  ;(displayln "\n--------------------------------------------------")
  ;(displayln (format "INFER ~a [~a]" goal bindings))
  (if (null? kb)
      empty-stream
      (let*([assertion (rename-variables (car kb))]
            [match (if (rule? assertion)
                       (unify goal
                              (single->nested (conclusion assertion))
                              bindings)
                       (unify goal
                              assertion
                              bindings))])
        ;(displayln (format "\t?? ~a" assertion))
        ;(displayln (format "\t== ~a" match))
        (if (equal? match 'fail)
            (infer goal bindings (cdr kb))
            (if (rule? assertion)
                (stream-append (solve (premise assertion) match)
                               (infer goal bindings (cdr kb)))
                (stream-cons match
                             (infer goal bindings (cdr kb))))))))

(define (apply-bindings pattern bindings)
  (cond [(constant? pattern) pattern]
        [(variable? pattern)
         (let ([binding (assoc pattern bindings)])
           (if binding
               (apply-bindings (cdr binding) bindings)
               pattern))]
        [(cons (apply-bindings (car pattern) bindings)
               (apply-bindings (cdr pattern) bindings))]))

; Printing
(define (print-solutions goal bindings-stream)
  (cond [(stream-empty? bindings-stream) '()]
        [(println (apply-bindings goal (stream-first bindings-stream)))
         (print-solutions goal (stream-rest bindings-stream))]))

; Test
(stream-for-each
 (lambda (s)
   (displayln "\n▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼")
   (displayln s)
   (displayln "▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲")
   (read-char))

 (stream-append
   ; SINGLE
   (solve '(("father" "P" "orestis")) '())
   (solve '(("father" "P" "O")) '())
   (solve '(("mother" "A" "O")) '())
   ; CONJUNCTIVE
   (solve '(("father" "P" "orestis") ("mother" "A" "orestis")) '())
   (solve '(("couple" "pavlos" "alkisti")) '())
 )

)
