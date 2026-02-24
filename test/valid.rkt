#lang racket

(require "../lib/ast.rkt")
(require "../lib/parse.rkt")
(require "../lib/valid.rkt")
(require rackunit)
(require "macros.rkt")

(check-equal? (replace-duplicates '(1 2 1) identity (λ (_) 3))

              '(1 2 3))

(check-equal? (replace-duplicates '(1 2 4) identity (λ (_) 3))

              '(1 2 4))

(check-equal? (replace-duplicates '((1 1) (1 2) (3 1)) second (λ (_) '(1 3)))

              '((1 1) (1 2) (1 3)))

(define EMPTY-PROG (prog '() '() 0.0))

(define CLASS-A-1 (class 'A
                    '(a b c)
                    '()))

(define CLASS-A-2 (struct-copy class CLASS-A-1
                               [field^* '(d e f)]))

(define CLASS-DUPLICATE-FIELDS (class 'A
                                 '(a b a)
                                 '()))

(define CLASS-DUPLICATE-FIELDS+DCME (struct-copy class CLASS-DUPLICATE-FIELDS
                                                 [field^* (list 'a 'b ERR-DUPLICATE-CLASS-MEMBERS)]))

(define EMPTY-SHAPE (shape '() '()))

(define METHOD-DUPLICATE-PARAMS (method 'm
                                        '(a b a)
                                        EMPTY-PROG))

(define METHOD-DUPLICATE-PARAMS+DCME (struct-copy method METHOD-DUPLICATE-PARAMS
                                                  [param^* (list 'a 'b ERR-DUPLICATE-CLASS-MEMBERS)]))

(define CLASS-DUPLICATE-PARAMS (class 'A
                                 '()
                                 (list METHOD-DUPLICATE-PARAMS)))

(define CLASS-DUPLICATE-PARAMS+DCME (struct-copy class CLASS-DUPLICATE-PARAMS
                                                 [method^* (list METHOD-DUPLICATE-PARAMS+DCME)]))

(define METHOD-M-1 (method 'm
                           '(a b c)
                           EMPTY-PROG))

(define METHOD-M-2 (struct-copy method METHOD-M-1
                                [param^* '(d e f)]))

(define METHOD-M-2+DCME (struct-copy method METHOD-M-2
                                     [name ERR-DUPLICATE-CLASS-MEMBERS]))

(define CLASS-DUPLICATE-METHODS (class 'A
                                  '()
                                  (list METHOD-M-1
                                        METHOD-M-2)))

(define CLASS-DUPLICATE-METHODS+DCME (struct-copy class CLASS-DUPLICATE-METHODS
                                                  [method^* (list METHOD-M-1
                                                                  METHOD-M-2+DCME)]))


(define (with-modules modules)
  (system modules '() (prog '() '() ($))))

(define (module-with-class c)
  (tmodule 'M '() c))

(check-equal? (check-duplicate-class-members (with-modules (list (module-with-class CLASS-DUPLICATE-FIELDS))))

              (with-modules (list (module-with-class CLASS-DUPLICATE-FIELDS+DCME))))

(check-equal? (check-duplicate-class-members (with-modules (list (module-with-class CLASS-DUPLICATE-PARAMS))))

              (with-modules (list (module-with-class CLASS-DUPLICATE-PARAMS+DCME))))

(check-equal? (check-duplicate-class-members (with-modules (list (module-with-class CLASS-DUPLICATE-METHODS))))

              (with-modules (list (module-with-class CLASS-DUPLICATE-METHODS+DCME))))

(check-equal? (check-undeclared (parse-system '((module M (class Point (x y)
                                                            (method delta (x)
                                                                    (def y (this --> y))
                                                                    (x = 1.0)
                                                                    (x + y))))
                                                (import M)
                                                (def x 1.0)
                                                (def point (new Point (x x)))
                                                (point --> x = x)
                                                (x = (point --> delta (x)))
                                                x)))

              (parse-system '((module M (class Point (x y)
                                          (method delta (x)
                                                  (def y (this --> y))
                                                  (x = 1.0)
                                                  (x + y))))
                              (import M)
                              (def x 1.0)
                              (def point (new Point (x x)))
                              (point --> x = x)
                              (x = (point --> delta (x)))
                              x)))



(define CLASS-A
  (class 'A '() '()))

(define METHOD-isaA
  (method 'isaA '(obj) (prog '() '() (isa 'obj 'A))))

(define METHOD-isaA+UVE
  (method 'isaA '(obj) (prog '() '() (isa 'obj ERR-UNDECLARED-VARIABLE))))

(define CLASS-B
  (class 'B '() (list METHOD-isaA)))

(define CLASS-B+UVE
  (struct-copy class CLASS-B
               [method^* (list METHOD-isaA+UVE)]))

(define MODULE-A
  (tmodule 'A '() CLASS-A EMPTY-SHAPE))

(define MODULE-A-CLASS-B
  (tmodule 'A '() CLASS-B EMPTY-SHAPE))

(define MODULE-A-CLASS-B+DMNE
  (struct-copy tmodule MODULE-A-CLASS-B
               [name ERR-DUPLICATE-MODULE-NAME]))

(define IMPORT-A
  (import 'A))

(define MODULE-B
  (tmodule 'B (list IMPORT-A) CLASS-B EMPTY-SHAPE))

(define MODULE-B-NO-IMPORTS
  (struct-copy tmodule MODULE-B
               [import^* '()]))

(define IMPORT-A+DCME
  (import ERR-UNDECLARED-VARIABLE))

(define IMPORT-B
  (import 'B))

(check-equal? (check-duplicate-module-names (with-modules (list MODULE-A MODULE-A-CLASS-B)))

              (with-modules (list MODULE-A MODULE-A-CLASS-B+DMNE)))

(check-equal? (lookup/module 'A (list MODULE-B MODULE-A))

              MODULE-A)

(check-equal? (lookup/module 'C (list MODULE-B MODULE-A))

              #f)

(check-equal? (check-undeclared/import IMPORT-A (list MODULE-B MODULE-A))

              IMPORT-A)

(check-equal? (check-undeclared/import IMPORT-A (list MODULE-B))

              IMPORT-A+DCME)

(check-equal-values? (check-undeclared/imports (list IMPORT-A IMPORT-B) (list MODULE-A MODULE-B))

                     ((list IMPORT-A IMPORT-B)
                      (list 'A 'B)))

(check-equal-values? (check-undeclared/imports (list IMPORT-A IMPORT-B)
                                               (list MODULE-A-CLASS-B MODULE-B))

                     ((list IMPORT-A IMPORT-B)
                      (list 'B 'B)))

(check-equal? (check-undeclared/module MODULE-B (list MODULE-A))

              MODULE-B)

(check-equal? (check-undeclared/module MODULE-B '())

              (struct-copy tmodule MODULE-B
                           [import^* (list IMPORT-A+DCME)]
                           [class    CLASS-B+UVE]))

(check-equal? (check-undeclared/module MODULE-B-NO-IMPORTS (list MODULE-A))

              (struct-copy tmodule MODULE-B-NO-IMPORTS
                           [class CLASS-B+UVE]))

(check-equal? (check-undeclared (parse-system '((module M (class C () (method val () 17.0)))
                                                (module N (class C
                                                            ()
                                                            (method val () 39.0)
                                                            (method go () (def x (new C ())) (x --> val ()))))
                                                (import M)
                                                (def y (new C ()))
                                                (y --> go ()))))

              (parse-system '((module M (class C () (method val () 17.0)))
                              (module N (class C
                                          ()
                                          (method val () 39.0)
                                          (method go () (def x (new C ())) (x --> val ()))))
                              (import M)
                              (def y (new C ()))
                              (y --> go ()))))

(check-equal? (check-undeclared/module (parse-tmodule '(module N (class C
                                                                  ()
                                                                  (method val () 39.0)
                                                                  (method go () (def x (new C ())) (x --> val ())))))
                                       '())

              (parse-tmodule '(module N (class C
                                         ()
                                         (method val () 39.0)
                                         (method go () (def x (new C ())) (x --> val ()))))))

(check-equal? (check-undeclared/class (parse-class '(class C
                                                      ()
                                                      (method val () 39.0)
                                                      (method go () (def x (new C ())) (x --> val ()))))
                                      '(C))

              (parse-class '(class C
                                                      ()
                                                      (method val () 39.0)
                                                      (method go () (def x (new C ())) (x --> val ())))))