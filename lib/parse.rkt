#lang racket

(require "ast.rkt")

(provide (all-defined-out))

;                                                         
;                                                         
;                                                         
;                                                         
;   ;;;;;;;       ;;;     ;;;;;;;      ;;;;;     ;;;;;;;; 
;   ;;;;;;;;      ;;;     ;;;;;;;;    ;;;;;;;    ;;;;;;;; 
;   ;;    ;;;     ;;;     ;;    ;;;  ;;;    ;    ;;       
;   ;;     ;;     ;;;     ;;     ;;  ;;          ;;       
;   ;;     ;;    ;; ;;    ;;     ;;  ;;;         ;;       
;   ;;    ;;;    ;; ;;    ;;    ;;;   ;;;;;      ;;;;;;;; 
;   ;;;;;;;;     ;; ;;    ;;;;;;;;     ;;;;;;    ;;;;;;;; 
;   ;;;;;;;      ;; ;;    ;;;;;;;          ;;;   ;;       
;   ;;          ;;;;;;;   ;;    ;;          ;;   ;;       
;   ;;          ;;;;;;;   ;;    ;;;         ;;   ;;       
;   ;;          ;;   ;;   ;;     ;;  ;     ;;;   ;;       
;   ;;         ;;     ;;  ;;     ;;  ;;;;;;;;    ;;;;;;;; 
;   ;;         ;;     ;;  ;;      ;;  ;;;;;;     ;;;;;;;; 
;                                                         
;                                                         
;                                                         
;                                                         

;; parse-system : ExampleGG -> AST+PE
(define (parse-system sexp)
  (match sexp
    [(list-rest (? ex-mmodule? mmodule^*) ...
                (list-rest (? ex-mimport? mimport^*) ...
                           (list-rest (? ex-decl? decl^*) ...
                                      (list-rest stmt^* ...
                                                 (list exp)))))
     (system (map parse-mmodule mmodule^*)
             (map parse-mimport mimport^*)
             (prog (map parse-decl decl^*)
                   (map parse-stmt stmt^*)
                   (parse-exp exp)))]
    [_ ERR-PROG-BAD-SHAPE]))

;; ex-gg? : [List-of Symbol], ExampleGG -> Boolean
(define (ex-gg? ss sexp)
  (and (cons? sexp)
       (symbol? (first sexp))
       (member (first sexp) ss)))

;; ex-mmodule? : ExampleGG -> Boolean
(define (ex-mmodule? sexp)
  (ex-gg? '(module tmodule) sexp))

;; ex-mimport? : ExampleGG -> Boolean
(define (ex-mimport? sexp)
  (ex-gg? '(import timport) sexp))

;; ex-decl? : ExampleGG -> Boolean
(define (ex-decl? sexp)
  (ex-gg? '(def) sexp))

;; parse-mmodule : ExampleGG -> MixedModule+PE
(define (parse-mmodule sexp)
  (match sexp
    [(cons 'module _) (parse-module sexp)]
    [(cons 'tmodule _) (parse-tmodule sexp)]
    [_ ERR-MIXED-MODULE-BAD-SHAPE]))

;; parse-module : ExampleGG -> Module+PE
(define (parse-module sexp)
  (match sexp
    [(list 'module name import^* ... class)
     (module (parse-variable name)
       (map parse-import import^*)
       (parse-class class))]
    [_ ERR-MODULE-BAD-SHAPE]))

;; parse-tmodule : ExampleGG -> TModule+PE
(define (parse-tmodule sexp)
  (match sexp
    [(list 'tmodule name mimport^* ... class shape)
     (module (parse-variable name)
       (map parse-mimport mimport^*)
       (parse-tclass class shape))]
    [_ ERR-TYPED-MODULE-BAD-SHAPE]))

;; parse-mimport : ExampleGG -> MixedImport+PE
(define (parse-mimport sexp)
  (match sexp
    [(cons 'import _) (parse-import sexp)]
    [(cons 'timport _) (parse-timport sexp)]
    [_ ERR-MIXED-IMPORT-BAD-SHAPE]))

;; parse-import : ExampleGG -> Import+PE
(define (parse-import sexp)
  (match sexp
    [(list 'import name)
     (import (parse-variable name) #f)]
    [_ ERR-IMPORT-BAD-SHAPE]))

;; parse-timport : ExampleGG -> TImport+PE
(define (parse-timport sexp)
  (match sexp
    [(list 'timport name shape)
     (import (parse-variable name) (parse-shape shape))]
    [_ ERR-TYPED-IMPORT-BAD-SHAPE]))

;; parse-class : ExampleGG -> Class+PE
(define (parse-class sexp)
  (match sexp
    [(list 'class name (list field^* ...) method^* ...)
     (class (parse-variable name)
       (map parse-variable field^*)
       (map (λ (m) (parse-method m name)) method^*)
       #f)]
                                                        
    [_ ERR-CLASS-BAD-SHAPE]))

;; parse-tclass : ExampleGG, ExampleGG -> TClass+PE
(define (parse-tclass sexp1 sexp2)
  (match sexp1
    [(list 'class name (list field^* ...) method^* ...)
     (class (parse-variable name)
       (map parse-variable field^*)
       (map (λ (m) (parse-method m name)) method^*)
       (parse-shape sexp2))]                     
    [_ ERR-CLASS-BAD-SHAPE]))

;; parse-shape : ExampleGG -> Shape+PE
(define (parse-shape sexp)
  (match sexp
    [(list (list field-type^* ...) (list method-type^* ...))
     (shape (map parse-field-type field-type^*)
            (map parse-method-type method-type^*))]
    [_ ERR-SHAPE-BAD-SHAPE]))

;; parse-field-type : ExampleGG -> FieldType+PE
(define (parse-field-type sexp)
  (match sexp
    [(list name type) (field-type (parse-variable name) (parse-type type))]
    [_ ERR-FIELD-TYPE-BAD-SHAPE]))

;; parse-method-type : ExampleGG -> MethodType+PE
(define (parse-method-type sexp)
  (match sexp
    [(list name (list type^* ...) type) (method-type (parse-variable name)
                                                     (map parse-type type^*)
                                                     (parse-type type))]
    [_ ERR-METHOD-TYPE-BAD-SHAPE]))


;; parse-type : ExampleGG -> Type+PE
(define (parse-type sexp)
  (match sexp
    ['Number 'number]
    [_       (parse-shape sexp)]))

;; parse-method : ExampleGG -> Method+PE
(define (parse-method sexp class-name)
  (match sexp
    [(list-rest 'method
                name
                (list param^* ...)
                (list-rest (? ex-decl? decl^*) ...
                           (list-rest stmt^* ...
                                      (list exp))))
     (method (parse-variable name)
             (map parse-variable param^*)
             (prog (map parse-decl decl^*)
                   (map parse-stmt stmt^*)
                   (parse-exp exp)))]
    [_ ERR-METHOD-BAD-SHAPE]))


;; parse-decl : ExampleGG -> Declaration+PE
(define (parse-decl sexp)
  (match sexp
    [(list 'def v exp) (def (parse-variable v)
                         (parse-exp exp))]
    [_                 ERR-DECL-BAD-SHAPE]))

;; parse-stmt : ExampleGG -> Statement+PE
(define (parse-stmt sexp)
  (match sexp
    [(list v '= exp)            (assignment (parse-variable v) (parse-exp exp))]
    [(list 'if0 exp b1 b2)      (if0 (parse-exp exp) (parse-block b1) (parse-block b2))]
    [(list 'while0 exp b)       (while0 (parse-exp exp) (parse-block b))]
    [(list v '--> field '= exp) (field-assign (parse-variable v)
                                              (parse-variable field)
                                              (parse-exp exp))]
    [_                          ERR-STATEMENT-BAD-SHAPE]))

;; parse-block : ExampleGG -> Block+PE
(define (parse-block sexp)
  (match sexp
    [(list 'block)                                         ERR-BLOCK-EMPTY]
    [(cons 'block (list-rest (? ex-decl? decl^*) ... '()))    ERR-BLOCK-EMPTY]
    [(cons 'block (list-rest (? ex-decl? decl^*) ... stmt^+)) (prog (map parse-decl decl^*)
                                                                    (map parse-stmt stmt^+)
                                                                    ($))]
    [(list 'block stmt^+ ...)                              (prog '()
                                                                 (map parse-stmt stmt^+)
                                                                 ($))]
    [stmt                                                  (parse-stmt stmt)]))

;; parse-exp : ExampleGG -> Expression+PE
(define (parse-exp sexp)
  (match sexp
    [(? number?)                  (parse-number sexp)]
    [(? symbol?)                  (parse-variable sexp)]
    [(list v1 '+ v2)              (add (parse-variable v1) (parse-variable v2))]
    [(list v1 '/ v2)              (div (parse-variable v1) (parse-variable v2))]
    [(list v1 '== v2)             (equality (parse-variable v1) (parse-variable v2))]
    [(list 'new name (list v^* ...))   (new (parse-variable name) (map parse-variable v^*))]
    [(list v '--> field)          (field-access (parse-variable v) (parse-variable field))]
    [(list v '--> name (list arg^* ...)) (method-call (parse-variable v)
                                                      (parse-variable name)
                                                      (map parse-variable arg^*))]
    [(list v 'isa name)           (isa (parse-variable v) (parse-variable name))]
    [_                            ERR-EXP-BAD-SHAPE]))

;; good-number? : Number -> Boolean
(define (parse-number n)
  (if (>= n -1000)
      (if (<= n 1000)
          n
          ERR-NUMBER-TOO-BIG)
      ERR-NUMBER-TOO-SMALL))

;; variable? : ExampleGG -> Variable
(define (parse-variable sexp)
  (if (symbol? sexp)
      (if (not (member sexp KEYWORDS))
          (if (<= (string-length (symbol->string sexp)) 20)
              (if (andmap (λ (letter) (member letter ALLOWED-CHARACTERS))
                          (explode (symbol->string sexp)))
                  sexp
                  ERR-NOT-ALPHABETIC)
              ERR-NAME-LENGTH-OVER-TWENTY)
          ERR-VARIABLE-IS-KEYWORD)
      ERR-VARIABLE-NOT-SYMBOL))