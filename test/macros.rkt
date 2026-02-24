#lang racket

(require rackunit)

(provide (all-defined-out))

(define-syntax-rule
  (check-equal-values? expression

                       (expected-result1 expected-result2))
  
  (let*-values ([(a b) expression])
    (check-equal? (list a b)

                  (list expected-result1 expected-result2))))