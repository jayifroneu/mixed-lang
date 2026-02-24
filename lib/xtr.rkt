#lang racket

(require "ast.rkt")
(require "parse.rkt")
(require "valid.rkt")
(require "type.rkt")
(require "link.rkt")
(require "cesk.rkt")

(provide (all-defined-out))

(define (xtr in)
     (define ast (parse-system in))
  
     (cond [(has-errors? ast)                                 "parser error"]
           [(has-errors? (check-duplicate-module-names ast))  "duplicate module name"]
           [(has-errors? (check-duplicate-class-members ast))
            "duplicate method, field, or parameter name"]
           [(has-errors? (check-bad-imports ast))             "bad import"]
           [(has-errors? (check-undeclared ast))              "undeclared variable error"]
           [(not (type-valid? ast))                           "type error"]
           [else (unload (call-with-values (λ () (load ast)) interpret))]))