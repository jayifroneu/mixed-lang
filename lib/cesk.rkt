#lang racket

(require racket/hash)
(require "ast.rkt")
(require "type.rkt")
(require "link.rkt")

(provide (all-defined-out))

;                                              
;                                              
;                                              
;                                              
;      ;;;;;    ;;;;;;;;    ;;;;;    ;;     ;; 
;     ;;;;;;;   ;;;;;;;;   ;;;;;;;   ;;    ;;  
;    ;;;    ;   ;;        ;;;    ;   ;;   ;;   
;    ;;         ;;        ;;         ;;  ;;    
;   ;;          ;;        ;;;        ;; ;;     
;   ;;          ;;;;;;;;   ;;;;;     ;;;;;     
;   ;;          ;;;;;;;;    ;;;;;;   ;;;;;     
;   ;;          ;;              ;;;  ;;  ;;    
;   ;;          ;;               ;;  ;;  ;;;   
;   ;;;         ;;               ;;  ;;   ;;   
;    ;;;    ;   ;;        ;     ;;;  ;;    ;;  
;     ;;;;;;;   ;;;;;;;;  ;;;;;;;;   ;;    ;;; 
;      ;;;;;    ;;;;;;;;   ;;;;;;    ;;     ;; 
;                                              
;                                              
;                                              
;                                              

;; A CESKState is a (state Control Environment Store Kontinuation)
(struct state [control env store ast] #:transparent)

;; A (C)ontrol is one of
;; - A dagger representing "I am searching for an expression"
(struct dagger [] #:transparent)
;; - Expression
;; - Value

;; An Environment is an immutable hash table and maps Variables to Locations

;; A Store is a mapping of Locations to Values

;; A Value is one of
;; - GoodNumber
;; - Object
;; - Proxy

;; A Proxy is a (proxy Object Shape)
(struct proxy [object shape])

;; conform : Value, Type, [List-of Class] -> Value+RE
(define (conform v type classes)
  (cond [(and (number? v) (symbol=? 'number type)) v]
        [(and (object? v) (shape? type))           (first-order-check v type classes)]
        [(and (proxy? v) (shape? type))            'check-proxy-against-shape]
        [else                                      ERR]))

(define (first-order-check obj s classes)
  (define field-type^*-sorted (sort (shape-field-type^* s) symbol<? #:key field-type-name))
  (define obj-pairs-sorted (sort (hash->list (object-field^* obj) symbol<? #:key first)))
  (define method^*-sorted (sort (class-method^* (lookup/class (object-name obj) classes)) symbol<?
                                #:key method-name))
  (define method-type^*-sorted (sort (shape-method-type^* s) symbol<? #:key method-type-name))
  
  (if (and (list=? (map field-type-name field-type^*-sorted)
               (map first obj-pairs-sorted)
               symbol=?)
       (andmap (negate string?) (map (λ (v type) (conform v type classes))
                                     (map rest obj-pairs-sorted)
                                     (map field-type-type field-type^*-sorted)))
       (list=? (map method-name method^*-sorted)
               (map method-type-name method-type^*-sorted)
               symbol=?)
       (andmap =
               (map (compose length method-param^*) method^*-sorted)
               (map (compose length method-type-param-type^*)method-type^*-sorted)))
      (proxy obj s)
      ERR))

(define (check-proxy-against-shape prx s)
  (if (type=? (proxy-shape prx) s)
      prx
      ERR))

;; An Object is a (object ClassName [Map-of Field -> Value])
(struct object [name [field^* #:mutable]] #:transparent)

(define (get obj name)
  (hash-ref (object-field^* obj) name))

(define (has-field? obj name)
  (hash-has-key? (object-field^* obj) name))

(define (assign! obj name v)
  (hash-set! (object-field^* obj) name v))

;; A Kontinuation is a [Stack-of Closure]

;; A Closure is a (closure Program Environment)
(struct closure [prog env] #:transparent)


(define DAGGER (dagger))
(define DOLLAR ($))
(define EPSILON 0.0000000001)
(define EMPTY-HASH (hash))
(define ZERO 0.0)
(define ONE 1.0)
(define ERR "run-time error")

;; load : AST -> CESKState, [List-of Class]
;; Consumes the AST of a Core program and produces an initial CESKState
(define (load ast)  
  (values (state DAGGER
                 EMPTY-HASH
                 EMPTY-HASH
                 (list (closure (link ast)
                                EMPTY-HASH)))
          (module^*->class^* (system-mmodule^* (synthesize ast)))))

;; lookup : Variable, Environment, Store -> GoodNumber
;; The value of the consumed variable for the consumed Environment and Store
(define (lookup v env st)
  (hash-ref st (hash-ref env v)))

;; lookup/class : ClassName, [List-of Class] -> Class
(define (lookup/class name classes)
  (findf (λ (c) (equal? name (class-name c)))
         classes))

;; lookup/method : MethodName, Class -> [Maybe Method]
(define (lookup/method name c)
  (findf (λ (m) (equal? name (method-name m)))
         (class-method^* c)))

;; core-eq? : Value, Value -> Boolean
;; Whether the two consumed GoodNumbers are equivalent in Core semantics
(define (core-eq? n1 n2)
  (match/values (values n1 n2)
                [((? number? n1) (? number? n2))
                 (< (abs (- n1 n2))
                    EPSILON)]
                [((object name1 field^*1) (object name2 field^*2))
                 (and (symbol=? name1 name2)
                      (field-map=? field^*1 field^*2))]
                [(_ _) #f]))

;; field-map=? : [Map-of Field -> Value], [Map-of Field -> Value] -> Boolean
(define (field-map=? f1 f2)
  (and (= (hash-count f1)
          (hash-count f2))
       (andmap (λ (p1 p2)
                 (and (symbol=? (car p1) (car p2))
                      (core-eq? (cdr p1) (cdr p2))))
               (hash->list f1 #t)
               (hash->list f2 #t))))

;; update : Store, Environment, Variable, GoodNumber -> Store
;; The consumed Store after updating the value of the location of the consumed Variable in the
;; consumed Environment to the consumed GoodNumber
(define (update st env v n)
  (hash-set st (hash-ref env v) n))

;; transition : CESKState, Location, [List-of Class] -> CESKState, Location
;; Consumes a non-final CSK state and produces the next state
(define (transition s l classes)
  (match s
    ;; Blocks and Declarations 
    ;; search ends with the rhs of a declaration
    [(state (? dagger?)
            env
            st
            (cons (closure (prog (cons (def x exp) def^*) stmt^* r)
                           env)
                  closure^*))
     (values (state exp
                    env
                    st
                    (cons (closure (prog (cons (def x exp) def^*) stmt^* r)
                                   env)
                          closure^*))
             l)]
    
    ;; value found for the right-hand side of declaration 
    [(state (or (? number? v)
                (? object? v))
            env
            st
            (cons (closure (prog (cons (def x exp) def^*) stmt^* r)
                           env)
                  closure^*))
     #:do[(define env-new (hash-set env x l))]
     (values (state DAGGER
                    env-new
                    (hash-set st l v)
                    (cons (closure (prog def^* stmt^* r)
                                   env-new)
                          closure^*))
             (add1 l))]
    
    ;; search encounters a nested block
    [(state (? dagger?)
            env
            st
            (cons (closure (prog '() (cons (prog def^* stmt^+ (? $?)) stmt^*) r)
                           env)
                  closure^*))
     (values (state DAGGER
                    env
                    st
                    (cons (closure (prog def^* stmt^+ DOLLAR)
                                   env)
                          (cons (closure (prog '() stmt^* r)
                                         env)
                                closure^*)))
             l)]
    
    ;; search exhausts a nested block
    [(state (? dagger?)
            env
            st
            (cons (closure (prog '() '() (? $?))
                           env)
                  (cons (closure p
                                 old-env)
                        closure^*)))
     (values (state DAGGER
                    old-env
                    st
                    (cons (closure p
                                   old-env)
                          closure^*))
             l)]
    
    ;; search ends with the return expression
    [(state (? dagger?)
            env
            st
            (cons (closure (prog '() '() r)
                           env)
                  closure^*))
     (values (state r
                    env
                    st
                    (cons (closure (prog '() '() r)
                                   env)
                          closure^*))
             l)]

    ;; search ends with evaluated return expression
    [(state (or (? number? v)
                (? object? v))
            env
            st
            (cons (closure (prog '() '() r)
                           env)
                  (cons (closure p old-env)
                        closure^*)))
     (values (state v
                    old-env
                    st
                    (cons (closure p old-env)
                          closure^*))
             l)]

    ;; Statements
    ;; search ends with right-hand side of assignment
    [(state (? dagger?)
            env
            st
            (cons (closure (prog '() (cons (assignment v exp) stmt^*) r)
                           env)
                  closure^*))
     (values (state exp
                    env
                    st
                    (cons (closure (prog '() (cons (assignment v exp) stmt^*) r)
                                   env)
                          closure^*))
             l)]
    
    ;; value for right-hand side of assignment
    [(state (or (? number? v)
                (? object? v))
            env
            st
            (cons (closure (prog '() (cons (assignment x exp) stmt^*) r)
                           env)
                  closure^*))
     (values (state DAGGER
                    env
                    (update st env x v)
                    (cons (closure (prog '() stmt^* r)
                                   env)
                          closure^*))
             l)]

    ;; search for expression in while
    [(state (? dagger?)
            env
            st
            (cons (closure (prog '() (cons (while0 exp b) stmt^*) r)
                           env)
                  closure^*))
     (values (state exp
                    env
                    st
                    (cons (closure (prog '() (cons (while0 exp b) stmt^*) r)
                                   env)
                          closure^*))
             l)]
    
    ;; decide whether to run while loop (positive)
    [(state (? number? n)
            env
            st
            (cons (closure (prog '() (cons (while0 exp b) stmt^*) r)
                           env)
                  closure^*))
     #:when (core-eq? ZERO n)
     (values (state DAGGER
                    env
                    st
                    (cons (closure (prog '() (cons b
                                                   (cons (while0 exp b) stmt^*))
                                         r)
                                   env)
                          closure^*))
             l)]
    
    ;; decide whether to run while loop (negative)
    [(state v
            env
            st
            (cons (closure (prog '() (cons (while0 exp b) stmt^*) r)
                           env)
                  closure^*))
     (values (state DAGGER
                    env
                    st
                    (cons (closure (prog '() stmt^* r) env)
                          closure^*))
             l)]
    
    ;; search for expression in if
    [(state (? dagger?)
            env
            st
            (cons (closure (prog '() (cons (if0 exp b1 b2) stmt^*) r)
                           env)
                  closure^*))
     (values (state exp
                    env
                    st
                    (cons (closure (prog '() (cons (if0 exp b1 b2) stmt^*) r)
                                   env)
                          closure^*))
             l)]

    ;; pick then branch from if0
    [(state (? number? n)
            env
            st
            (cons (closure (prog '() (cons (if0 exp b1 b2) stmt^*) r)
                           env)
                  closure^*))
     #:when (core-eq? 0 n)
     (values (state DAGGER
                    env
                    st
                    (cons (closure (prog '() (cons b1 stmt^*) r)
                                   env)
                          closure^*))
             l)]

    ;; pick else branch from if0
    [(state v
            env
            st
            (cons (closure (prog '() (cons (if0 exp b1 b2) stmt^*) r)
                           env)
                  closure^*))
     (values (state DAGGER
                    env
                    st
                    (cons (closure (prog '() (cons b2 stmt^*) r)
                                   env)
                          closure^*))
             l)]

    ;; Expressions
    ;; evaluate a variable
    [(state (? symbol? y) env st k)
     (values (state (lookup y env st) env st k)
             l)]

    ;; evaluate an addition (success)
    [(state (add v1 v2) env st k)
     #:when (number? (lookup v1 env st))
     #:when (number? (lookup v2 env st))
     (values (state (+ (lookup v1 env st) (lookup v2 env st)) env st k)
             l)]

    ;; evaluate an addition (failure)
    [(state (add v1 v2) env st k)
     (values (state ERR env st k)
             l)]

    ;; evaluate a comparison (positive)
    [(state (equality v1 v2) env st k)
     #:when (core-eq? (lookup v1 env st) (lookup v2 env st))
     (values (state ZERO env st k)
             l)]

    ;; evaluate a comparison (negative)
    [(state (equality v1 v2) env st k)
     (values (state ONE env st k)
             l)]

    ;; evaluate a division (success)
    [(state (div v1 v2) env st k)
     #:when (number? (lookup v1 env st))
     #:when (number? (lookup v2 env st))
     #:when (not (core-eq? ZERO (lookup v2 env st)))
     (values (state (/ (lookup v1 env st) (lookup v2 env st)) env st k)
             l)]

    ;; evaluate a division (failure)
    [(state (div v1 v2) env st k)
     (values (state ERR env st '())
             l)]

  
    ;; Objects: Creation and Inspection 

    ;; evaluate a ‘new‘ expression (untyped class, success)
    [(state (new name field^*) env st k)
     #:do [(define c (lookup/class name classes))]
     #:when (not (class-shape c))
     #:when (equal? (length (class-field^* c)) (length field^*))
     (values (state (object name
                            (make-hash (map (λ (field-name field)
                                              (cons field-name (lookup field env st)))
                                            (class-field^* c)
                                            field^*)))
                    env
                    st
                    k)
             l)]

    ;; evaluate a ‘new‘ expression (typed class, success)
    [(state (new name x^*) env st k)
     #:do [(define c (lookup/class name classes))]
     #:when (equal? (length (class-field^* c)) (length x^*))
     #:do [(define obj (object name
                               (make-hash (map (λ (field-name field)
                                                 (cons field-name (lookup field env st)))
                                               (class-field^* c)
                                               x^*))))]
     (values (state (conform obj (class-shape c) classes)
                    env
                    st
                    k)
             l)]
    ;; evaluate a ‘new‘ expression (failure)
    [(state (new name field^*) env st k)
     (values (state ERR env st k) l)]

    ;; evaluate an ‘isa‘ expression
    [(state (isa x name) env st k)
     #:do [(define obj (lookup x env st))]
     #:when (object? obj)
     (values (state (if (equal? (object-name obj) name)
                        ZERO
                        ONE)
                    env
                    st
                    k)
             l)]

    ;; Objects: Field Retrieval 
    ;; evaluate a field reference (object)
    [(state (field-access o f) env st k)
     #:do [(define obj (lookup o env st))]
     #:when (object? obj)
     #:when (has-field? obj f)
     (values (state (get obj f)
                    env st k)
             l)]

    ;; evaluate a field reference (proxy)
    [(state (field-access p f) e st k)
     #:do [(define prx (lookup p e st))]
     #:when (proxy? prx)
     #:do [(define ft (lookup/field-type f (proxy-shape prx)))]
     #:when ft
     #:do [(define obj (proxy-object prx))]
     #:do [(define u (get obj f))]
     (values (state (conform u (field-type-type ft) classes)
                    e st k)
             l)]

    ;; evaluate a field reference (failure)
    [(state (field-access v field) env st k)
     (values (state ERR env st k)
             l)]

    ;; Objects: Method Calls 
    ;; evaluate a method-call expression (object)
    [(state (method-call o m x^*) e st k)
     ;; subject to:
     ;; o is an object
     #:do [(define obj (lookup o e st))]
     #:when (object? obj)
     ;; has the desired method
     #:do [(define c (lookup/class (object-name obj) classes))]
     #:do [(define me (lookup/method m c))]
     #:when me
     ;; the correct number of parameters
     #:when (equal? (length x^*) (length (method-param^* me)))

     ;; after
     #:do [(define v^*               (map (λ (x) (lookup x e st)) x^*))
           (define param^*-of-m      (method-param^* me))
           (define number-of-param^* (length (method-param^* me)))
           (define this-l            (add1 l))
           (define new-ls            (build-list number-of-param^* (λ (n) (+ n l 2))))
           (define new-env           (hash-set (make-immutable-hash (map (λ (param^of-m new-l)
                                                                           (cons param^of-m new-l))
                                                                         param^*-of-m
                                                                         new-ls))
                                               'this
                                               this-l))
           (define new-st            (hash-set (make-immutable-hash (map (λ (new-l param-as-val)
                                                                           (cons new-l param-as-val))
                                                                         new-ls
                                                                         v^*))
                                               this-l
                                               obj))
           (define old-st-with-new-st (hash-union st new-st))
           (define body-of-m         (method-body me))] 

     (values (state DAGGER
                    new-env
                    old-st-with-new-st
                    (cons (closure body-of-m
                                   new-env) 
                          k))
             (+ l number-of-param^* 2))]

    ;; evaluate a method-call expression (proxy)
    [(state (method-call p m x^*) e st k)
     #:do[(define prx (lookup p e st))]
     #:when (proxy? prx)
     #:do [(define s (proxy-shape prx))]
     #:do [(define mt (lookup/method-type m s))]
     #:when mt
     #:do [(define obj (proxy-object prx))
           (define domain-type^* (method-type-param-type^* mt))
           (define range-type (method-type-type mt))]
     #:do [(define tmp^* (map (λ (x) (lookup x e st)) x^*))
           (define arg^* (map (λ (tmp) (conform tmp domain-type^* classes)) tmp^*))]
     #:when (andmap (negate string?) arg^*)
     #:do [(define c (lookup/class (object-name obj) classes))
           (define me (lookup/method m c))
           (define para^* (method-param^* me))
           (define body (method-body me))]
     #:do[(define number-of-param^* (length para^*))
          (define this-l            (add1 l))
          (define new-ls            (build-list number-of-param^* (λ (n) (+ n l 2))))
          (define e1                (hash-set (make-immutable-hash (map (λ (param^of-m new-l)
                                                                          (cons param^of-m new-l))
                                                                        para^*
                                                                        new-ls))
                                              'this
                                              this-l))
          (define new-st            (hash-set (make-immutable-hash (map (λ (new-l param-as-val)
                                                                          (cons new-l param-as-val))
                                                                        new-ls
                                                                        arg^*))
                                              this-l
                                              obj))
          (define s1 (hash-union st new-st))]
     (values (state DAGGER e1 s1
                    (cons (closure body e1)
                          (cons range-type
                                k)))
             (+ l number-of-param^* 2))]

    ;; returning from a proxied method call (success)
    [(state v e st (cons (? type? range-type) k))
     (values (state (conform v range-type classes) e st k)
             l)]

    ;; evaluate a method-call expression (failure)
    [(state (method-call v name param^*) env st k)
     (values (state ERR env st k)
             l)]

    

    ;; Objects: Field Mutation Statement 
    ;; search ends a field assignment
    [(state (? dagger?)
            env
            st
            (cons (closure (prog '() (cons (field-assign x f exp) stmt^*) r)
                           env)
                  closure^*))
     (values (state exp
                    env
                    st
                    (cons (closure (prog '() (cons (field-assign x f exp) stmt^*) r)
                                   env)
                          closure^*))
             l)]

    ;; execute a field assignment (object)
    [(state v
            env
            st
            (cons (closure (prog '() (cons (field-assign x f exp) stmt^*) r)
                           env)
                  closure^*))

     #:do   [(define obj (lookup x env st))]
     #:when (object? obj)
     #:when (has-field? obj f)
     #:do [(define change (assign! obj f v))]
     #;{#:do [(define change-field-map (hash-set! field-map f v))]
        #:do [(define change-object-field^* (set-object-field^*! obj field-map))]
        #:do [(define obj-l (hash-ref env x))]}
    
     (values (state DAGGER
                    env
                    st
                    (cons (closure (prog '() stmt^* r)
                                   env)
                          closure^*))
             l)]

    ;; execute a field assignment (proxy)
    [(state u e st
            (cons (closure (prog '()
                                 (cons (field-assign p f rhs)
                                       stmt^*)
                                 r)
                           e)
                  k))
     #:do[(define prx (lookup p e st))]
     #:when (proxy? prx)
     #:do[(define s (proxy-shape prx))
          (define ft (lookup/field-type f s))]
     #:when ft
     #:do[(define v (conform u (field-type-type ft) classes))]
     #:when (not (string? v))
     #:do [(define obj (proxy-object prx))
           (define change (assign! obj f v))]
     (values (state DAGGER e st
                    (cons (closure (prog '()
                                         stmt^*
                                         r)
                                   e)
                          k))
             l)]

    ;; execute a field assignment (failure)
    [(state v
            env
            st
            (cons (closure (prog '()
                                 (cons (field-assign var f exp) stmt^*) r)
                           env)
                  closure^*))
     (values (state ERR
                    env
                    st
                    (cons (closure (prog '()
                                         (cons (field-assign var f exp) stmt^*) r)
                                   env)
                          closure^*))
             l)]))

;; interpret : CESKState, [List-of Class] -> CESKState
;; the last CESKState that develops from the consumed CESKState
(define (interpret s classes)
  (define (interpret/accm s l classes)
    (match s
      [(state (or (? number?)
                  (? object?))
              env
              st
              (cons (closure (prog '() '() r)
                             env)
                    '()))                     s]
      [(state (? string?) env st k)           s]
      [(state c env st k)                     #:do[(define-values (s-next l-next)
                                                     (transition s l classes))]
                                              (interpret/accm s-next l-next classes)]))
  
  (interpret/accm s 0 classes))

;; unload : CESKState -> FinalState
;; Consumes a final CESK state and produces a FinalState that represents the output of the Class program
(define (unload s)
  (match s
    [(state (? number? n) env st k)   (exact->inexact n)]
    [(state (? object? obj) env st k) ERR]
    [(state (? string? err) env st k) ERR]))