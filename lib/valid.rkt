#lang racket

(require "ast.rkt")

(provide (all-defined-out))

;                                                         
;                                                         
;                                                         
;                                                         
;   ;;     ;;     ;;;     ;;          ;;;;;;;;  ;;;;;;    
;   ;;     ;;     ;;;     ;;          ;;;;;;;;  ;;;;;;;   
;    ;;   ;;      ;;;     ;;             ;;     ;;   ;;;  
;    ;;   ;;      ;;;     ;;             ;;     ;;    ;;; 
;    ;;   ;;     ;; ;;    ;;             ;;     ;;     ;; 
;    ;;; ;;;     ;; ;;    ;;             ;;     ;;     ;; 
;     ;; ;;      ;; ;;    ;;             ;;     ;;     ;; 
;     ;; ;;      ;; ;;    ;;             ;;     ;;     ;; 
;     ;; ;;     ;;;;;;;   ;;             ;;     ;;     ;; 
;      ;;;      ;;;;;;;   ;;             ;;     ;;    ;;; 
;      ;;;      ;;   ;;   ;;             ;;     ;;   ;;;  
;      ;;;     ;;     ;;  ;;;;;;;;;   ;;;;;;;;  ;;;;;;;   
;      ;;;     ;;     ;;  ;;;;;;;;;   ;;;;;;;;  ;;;;;;    
;                                                         
;                                                         
;                                                         
;                                                         


;                                              
;                                              
;                                              
;                                              
;   ;;;;;;     ;;     ;;  ;;;;;;;    ;;        
;   ;;;;;;;    ;;     ;;  ;;;;;;;;   ;;        
;   ;;   ;;;   ;;     ;;  ;;    ;;;  ;;        
;   ;;    ;;;  ;;     ;;  ;;     ;;  ;;        
;   ;;     ;;  ;;     ;;  ;;     ;;  ;;        
;   ;;     ;;  ;;     ;;  ;;    ;;;  ;;        
;   ;;     ;;  ;;     ;;  ;;;;;;;;   ;;        
;   ;;     ;;  ;;     ;;  ;;;;;;;    ;;        
;   ;;     ;;  ;;     ;;  ;;         ;;        
;   ;;    ;;;  ;;     ;;  ;;         ;;        
;   ;;   ;;;   ;;;   ;;;  ;;         ;;        
;   ;;;;;;;     ;;;;;;;   ;;         ;;;;;;;;; 
;   ;;;;;;       ;;;;;    ;;         ;;;;;;;;; 
;                                              
;                                              
;                                              
;                                              


;; replace-duplicates : {X, Y} [List-of X], [X -> Y], X -> [List-of X]
;; Produces a list in which all duplicate values that occur after any previous value (determined by
;; (equal? (extract value1) (extract value2) are replaced by a new value
(define (replace-duplicates l extract update)
  (define (replace-duplicates/accm l extract update seen)
    (match l
      ['()            (reverse seen)]
      [(cons fst rst) (replace-duplicates/accm rst
                                               extract
                                               update
                                               (cons (if (member (extract fst) (map extract seen))
                                                         (update fst)
                                                         fst)
                                                     seen))]))
  
  (replace-duplicates/accm l extract update '()))

;; check-duplicate-module-names : AST -> AST+DMNE
(define (check-duplicate-module-names ast)
  (struct-copy system ast
               [mmodule^* (replace-duplicates (system-mmodule^* ast)
                                              module-name
                                              (λ (mm)
                                                (struct-copy module mm
                                                             [name ERR-DUPLICATE-MODULE-NAME])))]))

;; check-duplicate-class-members : AST -> AST+DCME
(define (check-duplicate-class-members ast)
  (struct-copy system ast
               [mmodule^* (map check-duplicate-class-members/module (system-mmodule^* ast))]))

;; check-duplicate-class-members/module : TModule -> Module+DCME
(define (check-duplicate-class-members/module mm)
  (struct-copy module mm
               [class (check-duplicate-class-members/class (module-class mm))]))

;; check-duplicate-class-members/class : Class -> Class+DCME
(define (check-duplicate-class-members/class class-in)
  (match-define (class _ field^*-in method^*-in shape-in) class-in)
  
  (struct-copy class class-in
               [field^* (replace-duplicates field^*-in
                                            identity
                                            (λ (_) ERR-DUPLICATE-CLASS-MEMBERS))]
               [method^*
                (replace-duplicates (map check-duplicate-class-members/params
                                         method^*-in)
                                    method-name
                                    (λ (m) (struct-copy method m
                                                        [name ERR-DUPLICATE-CLASS-MEMBERS])))]
               [shape (and shape-in (check-duplicate-class-members/shape shape-in))]))

;; check-duplicate-class-members/params : Method -> Method+DCME
(define (check-duplicate-class-members/params method-in)
  (struct-copy method method-in
               [param^* (replace-duplicates (method-param^* method-in)
                                            identity
                                            (λ (_) ERR-DUPLICATE-CLASS-MEMBERS))]))

;; check-duplicate-class-members/shape : Shape -> Shape+DCME
(define (check-duplicate-class-members/shape shape-in)
  (match shape-in
    [(shape (list ft^* ...) (list mt^* ...))
     (shape (replace-duplicates ft^*
                                field-type-name
                                (λ (ft)
                                  (struct-copy field-type ft
                                               [name ERR-DUPLICATE-CLASS-MEMBERS])))
            (replace-duplicates mt^*
                                method-type-name
                                (λ (mt)
                                  (struct-copy method-type mt
                                               [name ERR-DUPLICATE-CLASS-MEMBERS]))))]))


;                                                                    
;                                                                    
;                                                                    
;                                                                    
;    ;;;;;;;; ;;;    ;;;  ;;;;;;;      ;;;;;    ;;;;;;;    ;;;;;;;;;;
;    ;;;;;;;; ;;;    ;;;  ;;;;;;;;    ;;;;;;;   ;;;;;;;;   ;;;;;;;;;;
;       ;;    ;;;    ;;;  ;;    ;;;   ;;   ;;   ;;    ;;;      ;;    
;       ;;    ;; ;  ; ;;  ;;     ;;  ;;     ;;  ;;     ;;      ;;    
;       ;;    ;; ;  ; ;;  ;;     ;;  ;;     ;;  ;;     ;;      ;;    
;       ;;    ;; ;;;; ;;  ;;    ;;;  ;;     ;;  ;;    ;;;      ;;    
;       ;;    ;;  ;;  ;;  ;;;;;;;;   ;;     ;;  ;;;;;;;;       ;;    
;       ;;    ;;  ;;  ;;  ;;;;;;;    ;;     ;;  ;;;;;;;        ;;    
;       ;;    ;;      ;;  ;;         ;;     ;;  ;;    ;;       ;;    
;       ;;    ;;      ;;  ;;         ;;     ;;  ;;    ;;;      ;;    
;       ;;    ;;      ;;  ;;          ;;   ;;   ;;     ;;      ;;    
;    ;;;;;;;; ;;      ;;  ;;          ;;;;;;;   ;;     ;;      ;;    
;    ;;;;;;;; ;;      ;;  ;;           ;;;;;    ;;      ;;     ;;    
;                                                                    
;                                                                    
;                                                                    
;                                                                    

;; lookup/module : ModuleName, [List-of MModule] -> [Maybe MModule]
(define (lookup/module name mmodules)
  (findf (λ (mm) (symbol=? name (module-name mm))) mmodules))

;; check-bad-imports : AST -> AST+BIE
(define (check-bad-imports ast)
  (define mmodule^*-in (system-mmodule^* ast))
  
  (struct-copy system ast
               [mmodule^* (check-bad-imports/modules mmodule^*-in)]
               [mimport^* (check-bad-imports/imports (system-mimport^* ast) mmodule^*-in)]))

;; check-bad-imports/modules : [List-of MModule] -> [List-of MModule+BIE]
(define (check-bad-imports/modules mmodule^*-in)
  (define (check-bad-imports/modules/accm mmodule^*-in mmodule^*-out)
    (match mmodule^*-in
      ['() (reverse mmodule^*-out)]
      [(cons mmodule-in rst)
       (check-bad-imports/modules/accm rst
                                       (cons (if (typed? mmodule-in)
                                                 (check-bad-imports/module mmodule-in mmodule^*-out)
                                                 mmodule-in)
                                             mmodule^*-out))]))

  (check-bad-imports/modules/accm mmodule^*-in '()))

;; check-bad-imports/module : TModule -> MModule+BIE
(define (check-bad-imports/module tmodule-in prev-mmodule^*)
  (struct-copy module tmodule-in
               [import^* (check-bad-imports/imports (module-import^* tmodule-in)
                                                    prev-mmodule^*)]))

(define (check-bad-imports/imports mimport^*-in prev-mmodule^*)
  (define module^* (filter (negate typed?) prev-mmodule^*))
  (define tmodule^* (filter typed? prev-mmodule^*))

  (define (check-bad-imports/imports/accm mimport^*-in mimport^*-out)
    (match mimport^*-in
      ['() (reverse mimport^*-out)]
      [(cons mimport-in rst)
       (check-bad-imports/imports/accm rst
                                       (cons (check-bad-imports/import mimport-in
                                                                       mimport^*-out
                                                                       module^*
                                                                       tmodule^*)
                                             mimport^*-out))]))

  (check-bad-imports/imports/accm mimport^*-in '()))

(define (check-bad-imports/import mimport-in prev-mimport^* prev-module^* prev-tmodule^*)
  (define name (import-name mimport-in))
  (define same-timport (findf (λ (mi) (and (not (err? mi))
                                           (symbol=? (import-name mi) name)))
                              prev-mimport^*))
  
  (cond [(and (lookup/module name prev-module^*)
              (not (typed? mimport-in)))                     ERR-BAD-IMPORT]
        [(and (lookup/module name prev-tmodule^*)
              (typed? mimport-in))                          ERR-BAD-IMPORT]
        [(and (typed? mimport-in)
              same-timport
              (not (shape=? (import-shape mimport-in)
                            (import-shape same-timport)))) ERR-BAD-IMPORT]
        [else                                               mimport-in]))


;                                                                    
;                                                                    
;                                                                    
;                                                                    
;   ;;     ;; ;;;     ;;  ;;;;;;      ;;;;;;;;     ;;;;;   ;;        
;   ;;     ;; ;;;     ;;  ;;;;;;;     ;;;;;;;;    ;;;;;;;  ;;        
;   ;;     ;; ;;;;    ;;  ;;   ;;;    ;;         ;;;    ;  ;;        
;   ;;     ;; ;; ;    ;;  ;;    ;;;   ;;         ;;        ;;        
;   ;;     ;; ;; ;;   ;;  ;;     ;;   ;;        ;;         ;;        
;   ;;     ;; ;;  ;   ;;  ;;     ;;   ;;;;;;;;  ;;         ;;        
;   ;;     ;; ;;  ;;  ;;  ;;     ;;   ;;;;;;;;  ;;         ;;        
;   ;;     ;; ;;   ;  ;;  ;;     ;;   ;;        ;;         ;;        
;   ;;     ;; ;;   ;; ;;  ;;     ;;   ;;        ;;         ;;        
;   ;;     ;; ;;    ; ;;  ;;    ;;;   ;;        ;;;        ;;        
;   ;;;   ;;; ;;    ;;;;  ;;   ;;;    ;;         ;;;    ;  ;;        
;    ;;;;;;;  ;;     ;;;  ;;;;;;;     ;;;;;;;;    ;;;;;;;  ;;;;;;;;; 
;     ;;;;;   ;;     ;;;  ;;;;;;      ;;;;;;;;     ;;;;;   ;;;;;;;;; 
;                                                                    
;                                                                    
;                                                                    
;                                                                    


;; check-undeclared : AST -> AST+UVE
(define (check-undeclared ast)
  (define mmodule^*+UVE (check-undeclared/modules (system-mmodule^* ast)))
  (define-values (mimport^*+UVE classes) (check-undeclared/imports (system-mimport^* ast)
                                                                   mmodule^*+UVE))
  (define prog+UVE (check-undeclared/prog (system-prog ast) classes '()))

  (system mmodule^*+UVE mimport^*+UVE prog+UVE))

;; check-undeclared/modules : [List-of TModule] -> [List-of TModule+UVE]
(define (check-undeclared/modules mmodule^*-in)
  (define (check-undeclared/modules/accm mmodule^*-in mmodule^*-out)
    (match mmodule^*-in
      ['() (reverse mmodule^*-out)]
      [(cons mmodule-in rst)
       (check-undeclared/modules/accm rst
                                      (cons (check-undeclared/module mmodule-in
                                                                     mmodule^*-out) mmodule^*-out))]))

  (check-undeclared/modules/accm mmodule^*-in '()))

;; check-undeclared/module : TModule, [List-ofT Module] -> TModule+UVE
(define (check-undeclared/module mmodule-in mmodules)
  (define-values (mimport^*+UVE classes) (check-undeclared/imports (module-import^* mmodule-in)
                                                                   mmodules))
  (define class-in (module-class mmodule-in))

  (struct-copy module mmodule-in
               [import^* mimport^*+UVE]
               [class    (check-undeclared/class class-in (cons (class-name class-in) classes))]))


;; check-undeclared/imports : [List-of Import], [List-of TModule] ->
;;                            [List-of Import+UVE], [List-of ClassName]

(define (check-undeclared/imports mimport^*-in mmodules)
  (define (check-undeclared/imports/accm mimport^*-in mimport^*-out mmodules classes)
    (match mimport^*-in
      ['() (values (reverse mimport^*-out)
                   (reverse classes))]
      [(cons mimport-in rst)
       #:do [(define r (lookup/module (import-name mimport-in) mmodules))]
       (check-undeclared/imports/accm rst
                                      (cons (check-undeclared/import mimport-in mmodules)
                                            mimport^*-out)
                                      mmodules
                                      (if r
                                          (cons (class-name (module-class r)) classes)
                                          classes))]))

  (check-undeclared/imports/accm mimport^*-in '() mmodules '()))

;; check-undeclared/import : Import, [List-of TModule] -> Import+UVE
(define (check-undeclared/import mimport-in mmodules)
  (struct-copy import mimport-in
               [name (check-undeclared/variable (import-name mimport-in)
                                                (map module-name mmodules))]))

;; check-undeclared/declaration : Declaration, [List-of ClassName], [List-of Variable] ->
;;                                Declaration+UVE
(define (check-undeclared/declaration decl-in classes variables)
  (struct-copy def decl-in
               [exp (check-undeclared/expression (def-exp decl-in) classes variables)]))

;; check-undeclared/declarations : [List-of Declaration], [List-of ClassName] ->
;;                                 [List-of Declaration+UVE], [List-of Variable]
(define (check-undeclared/declarations decl^*-in classes variables)
  (define (check-undeclared/declarations/accm decl^*-in decl^*-out classes variables)
    (match decl^*-in
      [(cons decl-in rst)
       (check-undeclared/declarations/accm rst
                                           (cons (check-undeclared/declaration decl-in
                                                                               classes variables)
                                                 decl^*-out)
                                           classes
                                           (cons (def-name decl-in) variables))]
      ['()
       (values (reverse decl^*-out) (reverse variables))]))
  (check-undeclared/declarations/accm decl^*-in '() classes variables))

;; check-undeclared/statements : [List-of Statement], [List-of ClassName], [List-of Variable] ->
;;                               [List-of Statement+UVE]
(define (check-undeclared/statements stmt^*-in classes variables)
  (map (λ (stmt-in) (check-undeclared/statement stmt-in classes variables)) stmt^*-in))

;; check-undeclared/variable : Variable, [List-of Variable] -> Variable+UVE
(define (check-undeclared/variable v variables)
  (if (member v variables)
      v
      ERR-UNDECLARED-VARIABLE))

;; check-undeclared/expression : Expression, [List-of ClassName], [List-of Variable] ->
;;                               Expression+UVE
(define (check-undeclared/expression exp-in classes variables)
  (match exp-in
    [(? number? n)                n]
    [(? symbol? v)                (check-undeclared/variable v variables)]
    [(add v1 v2)                  (add (check-undeclared/variable v1 variables)
                                       (check-undeclared/variable v2 variables))]
    [(div v1 v2)                  (div (check-undeclared/variable v1 variables)
                                       (check-undeclared/variable v2 variables))]
    [(equality v1 v2)             (equality (check-undeclared/variable v1 variables)
                                            (check-undeclared/variable v2 variables))]
    [(new name var^*)             (new (check-undeclared/variable name classes)
                                       (map (λ (v) (check-undeclared/variable v variables))
                                            var^*))]
    [(field-access v field)       (field-access (check-undeclared/variable v variables)
                                                field)]
    [(method-call v name param^*) (method-call (check-undeclared/variable v variables)
                                               name
                                               (map (λ (p)
                                                      (check-undeclared/variable p variables))
                                                    param^*))]
    [(isa v name)                 (isa (check-undeclared/variable v variables)
                                       (check-undeclared/variable name classes))]))

;; check-undeclared/return : Return, [List-of ClassName], [List-of Variable] -> Return+UVE
(define (check-undeclared/return return-in classes variables)
  (cond [($? return-in) ($)]
        [else           (check-undeclared/expression return-in classes variables)]))

;; check-undeclared/prog : Prog, [List-of ClassName], [List-of Variable] -> Prog+UVE
(define (check-undeclared/prog p classes variables-in)
  (define-values (decl^*+UVE variables-out) (check-undeclared/declarations (prog-decl^* p)
                                                                           classes
                                                                           variables-in))
  (define stmt^*+UVE (check-undeclared/statements (prog-stmt^* p) classes variables-out))
  (define r+UVE (check-undeclared/return (prog-exp p) classes variables-out))

  (prog decl^*+UVE stmt^*+UVE r+UVE))

;; check-undeclared/method : Method -> Method+UVE
(define (check-undeclared/method method-in classes)
  (struct-copy method method-in
               [body (check-undeclared/prog (method-body method-in)
                                            classes
                                            (cons 'this (method-param^* method-in)))]))

;; check-undeclared/class : Class, [List-of ClassName] -> Class+UVE
(define (check-undeclared/class class-in classes)
  (struct-copy class class-in
               [method^* (map (λ (m) (check-undeclared/method m classes))
                              (class-method^* class-in))]))

;; check-undeclared/statement : Statement, [List-of ClassName], [List-of Variable] -> Statement+UVE
(define (check-undeclared/statement stmt-in classes variables)
  (match stmt-in
    [(assignment v exp)         (assignment (check-undeclared/variable v variables)
                                            (check-undeclared/expression exp classes variables))]
    [(if0 exp b1 b2)            (if0 (check-undeclared/expression exp classes variables)
                                     (check-undeclared/block b1 classes variables)
                                     (check-undeclared/block b2 classes variables))]
    [(while0 exp block)         (while0 (check-undeclared/expression exp classes variables)
                                        (check-undeclared/block block classes variables))]
    [(field-assign v field exp) (field-assign (check-undeclared/variable v variables)
                                              field
                                              (check-undeclared/expression exp
                                                                           classes
                                                                           variables))]))

;; check-undeclared/block : Block, [List-of ClassName], [List-of Variable] -> Block+UVE
(define (check-undeclared/block block-in classes variables)
  (cond
    [(prog? block-in) (check-undeclared/prog block-in classes variables)]
    [else             (check-undeclared/statement block-in classes variables)]))



