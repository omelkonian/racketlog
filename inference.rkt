#lang racket
(require racket/stream)
(require racketlog/unification)
(provide run)

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
  (symbol->string (gensym "G#")))  

(define (rename expr replace-map)
  (cond [(constant? expr) expr]
        [(variable? expr) (cdr (assoc expr replace-map))]
        [(cons (rename (car expr) replace-map)
               (rename (cdr expr) replace-map))]))

; Inference
(define (solve goal bindings)
  ;(displayln "\n==================================================")
  ;(displayln (format "SOLVE ~a" goal))
  ;(displayln (format "\t[~a]" bindings))
  ;(displayln (format "\t{~a}" KB))
  (if (conjunctive? goal)
      (filter-goals goal (stream bindings))
      (infer goal bindings KB)))

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
  ;(displayln (format "INFER ~a" goal))
  ;(displayln (format "\t[~a]" bindings))
  ;(displayln (format "\t{~a}" kb))
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

; Printing
(define (external-variables? variable)
  (define identifier (car variable))
  (not (string-prefix? identifier "G#")))

(define (get-bottom-value id bindings)
  (define match (assoc id bindings))
  (if match (get-bottom-value (cdr match) bindings) id))

(define (show-solution solution)
  (displayln "\n▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼")
  (define user-variables (filter external-variables? solution))
  (if (null? user-variables)
      (displayln "True.")
      (map (lambda (entry)
             (define bottom (get-bottom-value (cdr entry) solution))         
             (displayln (format "~a = ~a" (car entry) bottom)))
           user-variables))
  (displayln "▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲"))
  
; Knowledge-base
(define KB null)

; Running
(define (run query knowledge-base)
  ;(displayln (format "RUN ~a" query))
  ;(displayln (format "\t {~a}" knowledge-base))
  (set! KB knowledge-base)
  (stream-for-each
   (lambda (solution)
     (show-solution solution)
     (read-char))
   (solve query '())))
