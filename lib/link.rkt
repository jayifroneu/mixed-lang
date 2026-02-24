#lang racket

(require "ast.rkt")
(require "valid.rkt")

(provide (all-defined-out))

;                                              
;                                              
;                                              
;                                              
;   ;;          ;;;;;;;; ;;;     ;;  ;;     ;; 
;   ;;          ;;;;;;;; ;;;     ;;  ;;    ;;  
;   ;;             ;;    ;;;;    ;;  ;;   ;;   
;   ;;             ;;    ;; ;    ;;  ;;  ;;    
;   ;;             ;;    ;; ;;   ;;  ;; ;;     
;   ;;             ;;    ;;  ;   ;;  ;;;;;     
;   ;;             ;;    ;;  ;;  ;;  ;;;;;     
;   ;;             ;;    ;;   ;  ;;  ;;  ;;    
;   ;;             ;;    ;;   ;; ;;  ;;  ;;;   
;   ;;             ;;    ;;    ; ;;  ;;   ;;   
;   ;;             ;;    ;;    ;;;;  ;;    ;;  
;   ;;;;;;;;;   ;;;;;;;; ;;     ;;;  ;;    ;;; 
;   ;;;;;;;;;   ;;;;;;;; ;;     ;;;  ;;     ;; 
;                                              
;                                              
;                                              
;

;; symbol-append : Symbol... -> Symbol
;; Like string-append, but for symbols.
(define (symbol-append . symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

(define (link ast)
  (match-define (system module^*-in import^*-in (prog decl^*-in stmt^*-in r)) (synthesize ast))

  (prog (map (λ (decl-in) (link/declaration decl-in import^*-in module^*-in)) decl^*-in)
        (map (λ (stmt-in) (link/statement stmt-in import^*-in module^*-in)) stmt^*-in)
        (link/return r import^*-in module^*-in)))

(define (synthesize ast)
  (match-define (system mmodule^*-in mimport^*-in prog-in) ast)
  (define-values (mimport^*-out tmodule^*-out) (synthesize/imports mimport^*-in mmodule^*-in 'Body))

  (struct-copy system ast
               [mmodule^* (append tmodule^*-out (synthesize/modules mmodule^*-in))]
               [mimport^* mimport^*-out]))

(define (synthesize/modules mmodule^*-in)
  (define (synthesize/modules/accm mmodule^*-in mmodule^*-out)
    (match mmodule^*-in
      ['() (reverse mmodule^*-out)]
      [(cons mmodule-in rst)
       #:do [(define-values (tmodule-out tmodule^*-out)
               (synthesize/module mmodule-in mmodule^*-out))]
       (synthesize/modules/accm rst
                                (append (cons tmodule-out mmodule^*-out) tmodule^*-out))]))

  (synthesize/modules/accm mmodule^*-in '()))

(define (synthesize/module mmodule-in prev-mmodule^*)
  (define-values (mimport^*-out tmodule^*-out)
    (synthesize/imports (module-import^* mmodule-in)
                        prev-mmodule^*
                        (module-name mmodule-in)))
  (define mmodule-out (struct-copy module mmodule-in
                                   [import^* mimport^*-out]))
  (values mmodule-out
          tmodule^*-out))

(define (synthesize/imports mimport^*-in prev-mmodule^* into-name)
  (define (synthesize/imports/accm mimport^*-in prev-mmodule^* into-name mimport^*-out tmodule^*-out)
    (match mimport^*-in
      ['() (values (reverse mimport^*-out) (reverse tmodule^*-out))]
      [(cons mimport-in rst)
       #:when (typed? mimport-in)
       #:do [(define-values (import-out tmodule-out) (synthesize/timport mimport-in
                                                                         prev-mmodule^*
                                                                         into-name))]
       (synthesize/imports/accm rst
                                prev-mmodule^*
                                into-name
                                (cons import-out
                                      mimport^*-out)
                                (cons tmodule-out
                                      tmodule^*-out))]
      [(cons mimport-in rst)
       (synthesize/imports/accm rst
                                prev-mmodule^*
                                into-name
                                (cons mimport-in
                                      mimport^*-out)
                                tmodule^*-out)]))

  (synthesize/imports/accm mimport^*-in prev-mmodule^* into-name '() '()))

(define (synthesize/timport timport-in prev-mmodule^* into-name)
  (define imported (lookup/module (import-name timport-in) prev-mmodule^*))
  (define import-out (struct-copy import timport-in
                                  [name (symbol-append (import-name timport-in)
                                                       '.into.
                                                       into-name)]
                                  [shape #f]))
  (define tmodule-out (struct-copy module imported
                                   [name (symbol-append (module-name imported)
                                                       '.into.
                                                       into-name)]
                                   [class (struct-copy class (module-class imported)
                                                       [shape (import-shape timport-in)])]))
  
  (values import-out
          tmodule-out))

;; module^*->class^* : [List-of Module] -> [List-of Class]
;; Maps a list of modules to a list of fully qualified classes.
(define (module^*->class^* module^*)
  (define (module^*->class^*/accm module^*-in prev-module^* class^*-out)
    (match module^*-in
      ['()          (reverse class^*-out)]
      [(cons m rst) (module^*->class^*/accm rst
                                            (cons m prev-module^*)
                                            (cons (module->class m prev-module^*) class^*-out))]))

  (module^*->class^*/accm module^* '() '()))

;; module->class : Module, [List-of Module] -> Class
;; Produces the fully qualified class that the consumed module exports, given a sequence of previously
;; declared modules.
(define (module->class module-in prev-module^*)
  (match module-in
    [(module m-name-in import^*-in (class c-name-in field^*-in method^*-in shape-in))
     (class (symbol-append m-name-in (string->symbol ".") c-name-in)
       field^*-in
       (map (λ (method-in)
              (link/method method-in
                           (add-to-last import^*-in (import m-name-in #f))
                           (add-to-last prev-module^* module-in)))
            method^*-in)
       #f)]))

(define (add-to-last l x)
  (append l (list x)))

;; link/method : Method, [List-of Import], [List-of Module]
;; Replaces each instance of a class name in the consumed method with its fully qualified name, given
;; a sequence of previous imports and a sequence of previous modules.
(define (link/method m import^* module^*)
  (match m
    [(method name param^* prog)
     (method name param^* (link/program prog import^* module^*))]))

;; link/program : Program, [List-of Module], [List-of Import] -> Program
;; Replaces each instance of a class name in the consumed program with its fully qualified name, given
;; a sequence of imports and modules.
(define (link/program p import^* module^*)
  (match p
    [(prog decl^* stmt^* r)
     (prog (map (λ (decl) (link/declaration decl import^* module^*)) decl^*)
           (map (λ (stmt) (link/statement stmt import^* module^*)) stmt^*)
           (link/return r import^* module^*))]))

(define (link/return r import^* module^*)
  (match r
    [($) r]
    [_   (link/expression r import^* module^*)]))


(define (link/declaration decl import^* module^*)
  (match decl
    [(def v exp) (def v (link/expression exp import^* module^*))]))


(define (link/statement stmt import^* module^*)
  (match stmt
    [(assignment var exp)        (assignment var (link/expression exp import^* module^*))]
    [(if0 exp b1 b2)             (if0 (link/expression exp import^* module^*)
                                      (link/block b1 import^* module^*)
                                      (link/block b2 import^* module^*))]
    [(while0 exp b)              (while0 (link/expression exp import^* module^*)
                                         (link/block b import^* module^*))]
    [(field-assign var name exp) (field-assign var name (link/expression exp import^* module^*))]))


(define (link/expression exp import^* module^*)
  (match exp
    [(new name var^*) (new (link/name name import^* module^*) var^*)]
    [(isa var name)   (isa var (link/name name import^* module^*))]
    [_                exp]))

(define (link/block b import^* module^*)
  (match b
    [(? prog?) (link/program b import^* module^*)]
    [_ (link/statement b import^* module^*)]))

;; link/name : ClassName, [List-of Import], [List-of Module] -> [Linked ClassName]
(define (link/name name import^* module^*)
  (define reversed-imports (reverse import^*))

  (define import-for-name (findf (λ (import)
                                   (equal? (class-name (module-class (lookup/module (import-name import) module^*)))
                                           name))
                                 reversed-imports))
  
  (symbol-append (import-name import-for-name) (string->symbol ".") name))