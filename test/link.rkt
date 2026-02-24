#lang racket

(require "../lib/ast.rkt")
(require "../lib/parse.rkt")
(require "../lib/link.rkt")
(require "macros.rkt")
(require rackunit)

#;{(define PROG-C (prog '() '() (new 'C '())))
   (define PROG-M.C (struct-copy prog PROG-C
                                 [exp (new 'M.C '())]))
   (define PROG-K.C (struct-copy prog PROG-C
                                 [exp (new 'K.C '())]))

   (define PROG-D (struct-copy prog PROG-C
                               [exp (new 'D '())]))

   (define PROG-L.D (struct-copy prog PROG-D
                                 [exp (new 'L.D '())]))

   (define METHOD-C (method 'm '() PROG-C))
   (define METHOD-M.C (struct-copy method METHOD-C
                                   [body PROG-M.C]))
   (define METHOD-K.C (struct-copy method METHOD-C
                                   [body PROG-K.C]))

   (define METHOD-D (struct-copy method METHOD-C
                                 [body PROG-D]))

   (define METHOD-L.D (struct-copy method METHOD-D
                                   [body PROG-L.D]))

   (define CLASS-C (class 'C '() (list METHOD-C)))

   (define CLASS-M.C (struct-copy class CLASS-C
                                  [name 'M.C]
                                  [method^* (list METHOD-M.C)]))

   (define CLASS-K.C (struct-copy class CLASS-C
                                  [name 'K.C]
                                  [method^* (list METHOD-K.C)]))

   (define CLASS-D (class 'D '() (list METHOD-C METHOD-D)))

   (define CLASS-L.D (struct-copy class CLASS-D
                                  [name 'L.D]
                                  [method^* (list METHOD-M.C METHOD-L.D)]))

   (define EMPTY-SHAPE (shape '() '()))

   #;{(define MODULE-M (tmodule 'M '() CLASS-C EMPTY-SHAPE))

      (define MODULE-K (tmodule 'K (list (import 'M)) CLASS-C EMPTY-SHAPE))

      (define MODULE-L (tmodule 'L (list (import 'M)) CLASS-D EMPTY-SHAPE))}

   (check-equal? (module->class MODULE-M '())

                 CLASS-M.C)

   (check-equal? (module->class MODULE-K (list MODULE-M))

                 CLASS-K.C)

   (check-equal? (module->class MODULE-L (list MODULE-M MODULE-K))

                 CLASS-L.D)

   (check-equal? (link/program PROG-C
                               (list (import 'K)
                                     (import 'M))
                               (list MODULE-M
                                     MODULE-K
                                     MODULE-L))

                 PROG-M.C)

   (check-equal? (link/program PROG-C
                               (list (import 'M)
                                     (import 'K))
                               (list MODULE-M
                                     MODULE-K
                                     MODULE-L))

                 PROG-K.C)



   (check-equal? (module^*->class^* (list MODULE-M
                                          MODULE-K
                                          MODULE-L))

                 (list CLASS-M.C
                       CLASS-K.C
                       CLASS-L.D))

   (check-equal? (symbol-append 'a 'b 'c)

                 'abc)

   (check-equal? (symbol-append 'a (string->symbol ".") 'c)

                 'a.c)

   (check-equal? (symbol-append 'abc 'bc 'c)

                 'abcbcc)}



(define C (class 'C '(a b) '() #f))
(define S (shape (list (field-type 'a 'number)
                       (field-type 'b 'number))
                 '()))
(define TC (struct-copy class C
                        [shape S]))

(define M (module 'M '() C))

(define TM (struct-copy module M
                        [class TC]))
(define M.INTO.BODY (struct-copy module TM
                                 [name 'M.into.Body]))

#;(check-equal? (synthesize (system (list M)
                                  (list (import 'M S))
                                  (prog '() '() ($))))

              (system (list M.INTO.BODY
                            M)
                      (list (import M.INTO.BODY #f))
                      (prog '() '() ($))))

(check-equal-values? (synthesize/imports (list (import 'M S))
                                         (list M)
                                         'Body)

                     ((list (import 'M.into.Body #f))
                      (list M.INTO.BODY)))