#lang racket

(require "../lib/ast.rkt")
(require "../lib/cesk.rkt")
(require "macros.rkt")
(require rackunit)


(define ONE 1.0)
(define TWO 2.0)
(define THREE 3.0)
(define X 'x)
(define Y 'y)
(define DEF-X-1 (def X ONE))
(define DEF-Y-2 (def Y TWO))
(define ENV-X->0 (hash-set EMPTY-HASH X 0))
(define ENV-X->0-Y->1 (hash-set ENV-X->0 Y 1))
(define ST-0->1 (hash-set EMPTY-HASH 0 ONE))
(define ST-0->1-1->2 (hash-set ST-0->1 1 TWO))
(define ST-0->1-1->0 (hash-set ST-0->1 1 ZERO))
(define ST-0->1-1->1 (hash-set ST-0->1 1 ONE))
(define X=2 (assignment X TWO))
(define Y=1 (assignment Y ONE))
(define X+Y (add X Y))
(define X/Y (div X Y))
(define X==X (equality X X))
(define X==Y (equality X Y))
(define EMPTY-BLOCK (prog '() '() DOLLAR))
(define B1 (prog '() (list X=2) DOLLAR))
(define B2 (prog (list DEF-Y-2) (list Y=1) DOLLAR))
(define WHILE-X-B1 (while0 X B1))
(define IF-X-B1-B2 (if0 X B1 B2))
(define PROG-X+Y (prog '() '() X+Y))
(define CLOSURE-X+Y (closure PROG-X+Y
                             EMPTY-HASH))

(define CLOSURE-DEF-X-1 (closure (prog (list DEF-X-1) '() X)
                                 EMPTY-HASH))

(define PROG-X (prog '() '() X))
(define CLOSURE-X (closure PROG-X
                           ENV-X->0))

(define PROG-ONE (prog '() '() ONE))
(define CLOSURE-ONE (closure PROG-ONE
                             ENV-X->0))
(define CLOSURE-X-Y (closure PROG-X
                             ENV-X->0-Y->1))
(define WHILE-Y-B1 (while0 Y B1))
(define PROG-WHILE-Y-B1 (prog '() (list WHILE-Y-B1) X))
(define CLOSURE-WHILE (closure PROG-WHILE-Y-B1
                               ENV-X->0-Y->1))
(define IF-0-B1-B2 (if0 ZERO B1 B2))
(define CLOSURE-IF-0-B1-B2 (closure (prog '() (list IF-0-B1-B2) X)
                                    ENV-X->0))

(define C (class 'C '(fieldOne fieldTwo)
            (list (method 'm '(n)
                          (prog (def 'nTwo (field-access 'this 'fieldOne))
                                '()
                                (add 'n 'nTwo))))
            #f))

(check-equal? (lookup/class 'C (list C))

              C)

(check-equal? (lookup/method 'm C)

              (method 'm '(n)
                      (prog (def 'nTwo (field-access 'this 'fieldOne))
                            '()
                            (add 'n 'nTwo))))

(check-equal-values? (load (system '() '() (prog '() '() 1.0)))

                     ((state (dagger)
                             EMPTY-HASH
                             EMPTY-HASH
                             (list (closure (prog '() '() 1.0) EMPTY-HASH)))
                      '()))

(define Z 'z)

(define (mut-hash-with ht assocs)
  (for-each (λ (assoc)
              (hash-set! ht (first assoc) (second assoc)))
            assocs))

(define Z-FIELD-MAP (make-hash))

(mut-hash-with Z-FIELD-MAP (list (list 'fieldOne ONE) (list 'fieldTwo TWO)))

(define Z-FIELD-MAP/2 (hash-copy Z-FIELD-MAP))

(hash-set! Z-FIELD-MAP/2 'fieldOne THREE)

(define OBJ/2 (object 'C Z-FIELD-MAP/2))

(define OBJ (object 'C Z-FIELD-MAP))
(define ENV-Z->0 (hash-set EMPTY-HASH Z 0))
(define ST-0->OBJ (hash-set EMPTY-HASH 0 OBJ))



;; BLOCKS AND DEFINITIONS
;; search ends with def rhs
(check-equal-values? (transition (state DAGGER
                                        EMPTY-HASH
                                        EMPTY-HASH
                                        (list CLOSURE-DEF-X-1))
                                 0
                                 '())

                     ((state ONE
                             EMPTY-HASH
                             EMPTY-HASH
                             (list CLOSURE-DEF-X-1))
                      0))

;; value for right-hand side of declaration
(check-equal-values? (transition (state ONE
                                        EMPTY-HASH
                                        EMPTY-HASH
                                        (list CLOSURE-DEF-X-1))
                                 0
                                 '())

                     ((state DAGGER
                             ENV-X->0
                             ST-0->1
                             (list (closure (prog '() '() X)
                                            ENV-X->0)))
                      1))

;; search encounters nested block
(check-equal-values? (transition (state DAGGER
                                        ENV-X->0
                                        ST-0->1
                                        (list (closure (prog '() (list B2) X)
                                                       ENV-X->0)))
                                 1
                                 '())

                     ((state DAGGER
                             ENV-X->0
                             ST-0->1
                             (list (closure B2
                                            ENV-X->0)
                                   CLOSURE-X))
                      1))

;; search exhausts nested block
(check-equal-values? (transition (state DAGGER
                                        ENV-X->0-Y->1
                                        ST-0->1-1->2
                                        (list (closure EMPTY-BLOCK
                                                       ENV-X->0-Y->1)
                                              CLOSURE-X))
                                 0
                                 '())

                     ((state DAGGER
                             ENV-X->0
                             ST-0->1-1->2
                             (list CLOSURE-X))
                      0))

;; search ends with the return expression
(check-equal-values? (transition (state DAGGER
                                        ENV-X->0
                                        EMPTY-HASH
                                        (list CLOSURE-X))
                                 0
                                 '())

                     ((state X
                             ENV-X->0
                             EMPTY-HASH
                             (list CLOSURE-X))
                      0))


;; EXPRESSIONS
;; evaluate a variable
(check-equal-values? (transition (state X
                                        ENV-X->0
                                        ST-0->1
                                        (list CLOSURE-X))
                                 0
                                 '())
                     
                     ((state ONE
                             ENV-X->0
                             ST-0->1
                             (list CLOSURE-X))
                      0))

;; evaluate an addition
(check-equal-values? (transition (state X+Y
                                        ENV-X->0-Y->1
                                        ST-0->1-1->2
                                        (list CLOSURE-X-Y))
                                 0
                                 '())
                     
                     ((state 3.0
                             ENV-X->0-Y->1
                             ST-0->1-1->2
                             (list CLOSURE-X-Y))
                      0))

;; evaluate a division (success)
(check-equal-values? (transition (state X/Y
                                        ENV-X->0-Y->1
                                        ST-0->1-1->2
                                        (list CLOSURE-X-Y))
                                 0
                                 '())
                     
                     ((state 0.5
                             ENV-X->0-Y->1
                             ST-0->1-1->2
                             (list CLOSURE-X-Y))
                      0))

;; evaluate a division (failure)
(check-equal-values? (transition (state X/Y
                                        ENV-X->0-Y->1
                                        ST-0->1-1->0
                                        (list (closure PROG-X
                                                       ENV-X->0-Y->1)))
                                 0
                                 '())
                     
                     ((state ERR
                             ENV-X->0-Y->1
                             ST-0->1-1->0
                             '())
                      0))

;; evaluate a comparison (positive)
(check-equal-values? (transition (state X==Y
                                        ENV-X->0-Y->1
                                        ST-0->1-1->1
                                        (list CLOSURE-X-Y))
                                 0
                                 '())
                     
                     ((state ZERO
                             ENV-X->0-Y->1
                             ST-0->1-1->1
                             (list CLOSURE-X-Y))
                      0))

;; evaluate a comparison (negative)
(check-equal-values? (transition (state X==Y
                                        ENV-X->0-Y->1
                                        ST-0->1-1->2
                                        (list CLOSURE-X-Y))
                                 0
                                 '())
                     
                     ((state ONE
                             ENV-X->0-Y->1
                             ST-0->1-1->2
                             (list CLOSURE-X-Y))
                      0))

;; evaluate a ‘new‘ expression (success)
(check-equal-values? (transition (state (new 'C (list X Y))
                                        ENV-X->0-Y->1
                                        ST-0->1-1->2
                                        (list (closure PROG-ONE
                                                       ENV-X->0-Y->1)))
                                 0
                                 (list C))

                     ((state OBJ
                             ENV-X->0-Y->1
                             ST-0->1-1->2
                             (list (closure PROG-ONE
                                            ENV-X->0-Y->1)))
                      0))

;; evaluate an ‘isa‘ expression (positive)
(check-equal-values? (transition (state (isa Z 'C)
                                        ENV-Z->0
                                        ST-0->OBJ
                                        (list (closure PROG-ONE
                                                       ENV-Z->0)))
                                 1
                                 (list C))

                     ((state ZERO
                             ENV-Z->0
                             ST-0->OBJ
                             (list (closure PROG-ONE
                                            ENV-Z->0)))
                      1))

(define D (struct-copy class C
                       [name 'D]))

;; evaluate an ‘isa‘ expression (negative)
(check-equal-values? (transition (state (isa Z 'D)
                                        ENV-Z->0
                                        ST-0->OBJ
                                        (list (closure PROG-ONE
                                                       ENV-Z->0)))
                                 1
                                 (list C D))

                     ((state ONE
                             ENV-Z->0
                             ST-0->OBJ
                             (list (closure PROG-ONE
                                            ENV-Z->0)))
                      1))

;; evaluate a field reference (success)
(check-equal-values? (transition (state (field-access Z 'fieldOne)
                                        ENV-Z->0
                                        ST-0->OBJ
                                        (list (closure PROG-ONE
                                                       ENV-Z->0)))
                                 0
                                 (list C))

                     ((state ONE
                             ENV-Z->0
                             ST-0->OBJ
                             (list (closure PROG-ONE
                                            ENV-Z->0)))
                      0))

;; evaluate a field reference (failure)
(check-equal-values? (transition (state (field-access Z 'fieldThree)
                                        ENV-Z->0
                                        ST-0->OBJ
                                        (list (closure PROG-ONE
                                                       ENV-Z->0)))
                                 0
                                 (list C))

                     ((state ERR
                             ENV-Z->0
                             ST-0->OBJ
                             (list (closure PROG-ONE
                                            ENV-Z->0)))
                      0))

(define ENV-Z->0-X->1 (hash-set ENV-Z->0 X 1))
(define ST-0->OBJ-1->1 (hash-set ST-0->OBJ 1 ONE))



;; evaluate a method-call expression (success)
(check-equal-values? (transition (state (method-call Z 'm (list X))
                                        ENV-Z->0-X->1
                                        ST-0->OBJ-1->1
                                        (list (closure PROG-ONE
                                                       ENV-Z->0-X->1)))
                                 2
                                 (list C))

                     ((state DAGGER
                             (hash 'n 4 'this 3)
                             (hash 0 OBJ 1 ONE 3 OBJ 4 ONE)
                             (list (closure (prog (def 'nTwo (field-access 'this 'fieldOne))
                                                  '()
                                                  (add 'n 'nTwo))
                                            (hash 'n 4 'this 3))
                                   (closure PROG-ONE
                                            ENV-Z->0-X->1)))
                      5))

;; evaluate a method-call expression (success)
(check-equal-values? (transition (state (method-call Z 'g (list X))
                                        ENV-Z->0-X->1
                                        ST-0->OBJ-1->1
                                        (list (closure PROG-ONE
                                                       ENV-Z->0-X->1)))
                                 2
                                 (list C))

                     ((state ERR
                             ENV-Z->0-X->1
                             ST-0->OBJ-1->1
                             (list (closure PROG-ONE
                                            ENV-Z->0-X->1)))
                      2))


;; ASSIGNMENT STATEMENTS
;; search ends with right-hand side of assignment
(check-equal-values? (transition (state DAGGER
                                        ENV-X->0-Y->1
                                        ST-0->1-1->2
                                        (list (closure (prog '() (list Y=1) X)
                                                       ENV-X->0-Y->1)))
                                 0
                                 '())

                     ((state ONE
                             ENV-X->0-Y->1
                             ST-0->1-1->2
                             (list (closure (prog '() (list Y=1) X)
                                            ENV-X->0-Y->1)))
                      0))

;; value for right-hand side of assignment
(check-equal-values? (transition (state ONE
                                        ENV-X->0-Y->1
                                        ST-0->1-1->2
                                        (list (closure (prog '() (list Y=1) X)
                                                       ENV-X->0-Y->1)))
                                 0
                                 '())
                                 

                     ((state DAGGER
                             ENV-X->0-Y->1
                             ST-0->1-1->1
                             (list (closure PROG-X
                                            ENV-X->0-Y->1)))
                      0))

;; search ends a field assignment
(check-equal-values? (transition (state DAGGER
                                        ENV-Z->0
                                        ST-0->OBJ
                                        (list (closure (prog '() (list (field-assign Z 'fieldOne THREE)) ONE)
                                                       ENV-Z->0)))
                                 1
                                 (list C))

                     ((state THREE
                             ENV-Z->0
                             ST-0->OBJ
                             (list (closure (prog '() (list (field-assign Z 'fieldOne THREE)) ONE)
                                            ENV-Z->0)))
                      1))

;; execute a field assignment (success)
(check-equal-values? (transition (state THREE
                                        ENV-Z->0
                                        ST-0->OBJ
                                        (list (closure (prog '() (list (field-assign Z 'fieldOne THREE)) ONE)
                                                       ENV-Z->0)))
                                 1
                                 (list C))

                     ((state DAGGER
                             ENV-Z->0
                             (hash-set EMPTY-HASH 0 OBJ/2)
                             (list (closure (prog '() '() ONE)
                                            ENV-Z->0)))
                      1))

;; WHILE LOOPS
;; search for expression in while
(check-equal-values? (transition (state DAGGER
                                        ENV-X->0-Y->1
                                        ST-0->1-1->0
                                        (list CLOSURE-WHILE))
                                 0
                                 '())

                     ((state Y
                             ENV-X->0-Y->1
                             ST-0->1-1->0
                             (list CLOSURE-WHILE))
                      0))

;; decide whether to run while loop (positive)
(check-equal-values? (transition (state ZERO
                                        ENV-X->0-Y->1
                                        ST-0->1-1->0
                                        (list CLOSURE-WHILE))
                                 0
                                 '())

                     ((state DAGGER
                             ENV-X->0-Y->1
                             ST-0->1-1->0
                             (list (closure (prog '() (list B1 WHILE-Y-B1) X)
                                            ENV-X->0-Y->1)))
                      0))

;; decide whether to run while loop (negative)
(check-equal-values? (transition (state ONE
                                        ENV-X->0-Y->1
                                        ST-0->1-1->0
                                        (list CLOSURE-WHILE))
                                 0
                                 '())

                     ((state DAGGER
                             ENV-X->0-Y->1
                             ST-0->1-1->0
                             (list (closure PROG-X
                                            ENV-X->0-Y->1)))
                      0))

;; CONDITIONALS
;; search for expression in if
(check-equal-values? (transition (state DAGGER
                                        ENV-X->0
                                        ST-0->1
                                        (list CLOSURE-IF-0-B1-B2))
                                 0
                                 '())

                     ((state ZERO
                             ENV-X->0
                             ST-0->1
                             (list CLOSURE-IF-0-B1-B2))
                      0))

;; pick then branch from if0
(check-equal-values? (transition (state ZERO
                                        ENV-X->0
                                        ST-0->1
                                        (list CLOSURE-IF-0-B1-B2))
                                 0
                                 '())

                     ((state DAGGER
                             ENV-X->0
                             ST-0->1
                             (list (closure (prog '() (list B1) X)
                                            ENV-X->0)))
                      0))

;; pick else branch from if0
(check-equal-values? (transition (state ONE
                                        ENV-X->0
                                        ST-0->1
                                        (list CLOSURE-IF-0-B1-B2))
                                 0
                                 '())

                     ((state DAGGER
                             ENV-X->0
                             ST-0->1
                             (list (closure (prog '() (list B2) X)
                                            ENV-X->0)))
                      0))

(check-equal? (interpret (state 1
                                ENV-X->0
                                ST-0->1
                                (list CLOSURE-ONE))
                         '())

              (state 1
                     ENV-X->0
                     ST-0->1
                     (list CLOSURE-ONE)))

(check-equal? (interpret (state ERR
                                ENV-X->0
                                ST-0->1
                                (list CLOSURE-X))
                         '())

              (state ERR
                     ENV-X->0
                     ST-0->1
                     (list CLOSURE-X)))

(check-equal? (interpret (state X+Y
                                ENV-X->0-Y->1
                                ST-0->1-1->2
                                (list (closure PROG-X+Y
                                               ENV-X->0-Y->1)))
                         '())

              (state THREE
                     ENV-X->0-Y->1
                     ST-0->1-1->2
                     (list (closure PROG-X+Y
                                    ENV-X->0-Y->1))))

(check-equal? (interpret (state 5.0
                                EMPTY-HASH
                                EMPTY-HASH
                                (list (closure (prog '() '() 5.0)
                                               EMPTY-HASH)))
                         '())

              (state 5.0
                     EMPTY-HASH
                     EMPTY-HASH
                     (list (closure (prog '() '() 5.0)
                                    EMPTY-HASH))))

(check-equal? (unload (state ERR EMPTY-HASH EMPTY-HASH (list CLOSURE-X)))

              ERR)

(check-within (unload (state ONE EMPTY-HASH EMPTY-HASH (list CLOSURE-X)))

              1 0.001)

(check-equal? (unload (state (object 'a EMPTY-HASH) EMPTY-HASH EMPTY-HASH (list CLOSURE-X)))

              "run-time error")

(define-simple-check (check=? v1 v2 =?)
  (=? v1 v2))

(check=? ONE ONE core-eq?)

(check=? OBJ OBJ core-eq?)

(check=? OBJ OBJ/2 core-eq?)

(check=? OBJ ONE (negate core-eq?))