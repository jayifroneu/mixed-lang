#lang racket

(require "../lib/ast.rkt")
(require "../lib/parse.rkt")
(require rackunit)

(check-pred (negate has-errors?)

            (parse-system '((x = 1.0)
                            (y = 2.0)
                            (if0 (x == y)
                                 (block (z = 3.0))
                                 (block (z = -3.0)))
                            z)))

(check-pred (negate has-errors?)

            (parse-system '((def a 4.0)
                            (x = 1.0)
                            (y = 2.0)
                            (if0 (x == y)
                                 (block (z = 3.0))
                                 (block (z = -3.0)))
                            z)))

(check-pred (negate has-errors?)

            (parse-system '((def a 4.0)
                            (x = 1.0)
                            (y = 2.0)
                            (if0 (x == y)
                                 (block (def d 10.0) (z = 3.0))
                                 (block (z = -3.0)))
                            z)))

(check-pred has-errors?

            (parse-system '(3 3)))

(check-pred has-errors?

            (parse-system '((class className))))

#;(check-pred (negate has-errors?)

              (parse-system '((module M (class c
                                          (f)
                                          (method m (p) ONE)))
                              (import M)
                              (def x ZERO)
                              (x = ONE)
                              x)))

#;(check-pred (negate has-errors?)

              (parse-system '((module M (class c
                                          (f)
                                          (method m (p) ONE)))
                              (import M)
                              (def instance (new c (fieldOne)))
                              (def x ZERO)
                              (def y ZERO)
                              (def z ZERO)
                              (x = ONE)
                              (instance --> fieldOne = 5.0)
                              (y = (instance --> fieldOne))
                              (z = (instance --> m (x)))
                              (z = (instance isa c))
                              z)))

#;(check-pred system?

              (parse-system '((class c
                                (f)
                                (method m (p) ONE))
                              (def x ZERO)
                              (x = ONE)
                              x)))

(check-pred system?

            (parse-system '(z)))

(check-pred system?

            (parse-system '((def x 1.0)
                            (def y 2.0)
                            (if0 (x == y)
                                 (block (def z 3.0)
                                        (x = 0))
                                 (block (def z -3.0)
                                        (y = 1)))
                            z)))

(check-pred err?

            (parse-system '()))

(check-pred system?

            (parse-system '((def x ONE)
                            (class c (f)
                              (method m (p) ONE))
                            ONE)))

;; This program is ill-formed, but the error is produced when the "statements" after the first
;; definition are parsed and the parser encounters the second definition where it shouldn't.
(check-pred system?

            (parse-system '((def x 1)
                            (x = 1)
                            (def y 2)
                            y)))

;; Similarly, the parser sees no leading declarations, assumes everything following the first
;; statement (sans final expression) is also a statement, a produces an error when it finds a
;; declaration.
(check-pred system?

            (parse-system '((x = 1)
                            (def y 2)
                            (y = 2)
                            y)))

#;(check-equal? (parse-class '(class c
                                (fOne fTwo)
                                (method mOne () 1.0)
                                (method mTwo () 2.0)))
              
                (class 'c (list 'fOne 'fTwo) (list (parse-method '(method mOne () 1.0) 'c)
                                                   (parse-method '(method mTwo () 2.0) 'c))))

;; This class is ill-formed, but the error is produced when the field parsing happens and it finds
;; a method instead. Also, when the methods are parsed, it will find a field and produce an error 
(check-pred class?

            (parse-class '(class c
                            (method mOne () 1.0)
                            (fOne))))

;; This class is ill-formed, but the error is produced when the field parsing happens and it finds
;; a method instead. Also, when the methods are parsed, it will find a field and produce an error 
(check-pred class?

            (parse-class '(class c
                            (method mOne () 1.0)
                            (fOne))))
                 

(check-equal? (class? '())

              #f)

(check-equal? (class? '(method))

              #f)

(check-equal? (class? '(class 1))

              #f)

(check-equal? (parse-method '(method name (paramOne paramTwo) (def x 2.0) (x = 3.0) 1.0) 'c)

              (method 'name
                      '(paramOne paramTwo)
                      (prog (list (def 'x 2.0)) (list (assignment 'x 3.0)) 1.0)))

(check-equal? (parse-method '(method name () 1) 'a)

              (method 'name
                      '()
                      (prog '() '() 1)))

(check-equal? (parse-method '(method add (nOne nTwo)
                                     (def x nOne)
                                     (def y nTwo)
                                     (x + y))

                            'c)
              
              (method 'add '(nOne nTwo)
                      (prog (list (def 'x 'nOne)
                                  (def 'y 'nTwo))
                            '()
                            (add 'x 'y))))

(check-equal? (ex-decl? 'x)

              #f)

(check-equal? (ex-decl? '())

              #f)

(check-pred (negate ex-decl?)

            '(if0 0 (def x 1)))

(check-pred ex-decl?

            '(def x 1))

;; While these declarations are ill-formed, we can at least tell that they are attempting to *be*
;; declarations, so the program as a whole has the correct shape. The function that parses
;; declarations will turn these into appropriate error nodes.
(check-pred ex-decl?

            '(def))

(check-pred ex-decl?

            '(def x))

(check-equal? (parse-decl '(def x 1))

              (def 'x 1))

(check-equal? (parse-decl '(def))

              ERR-DECL-BAD-SHAPE)

(check-equal? (parse-decl '(def X))

              ERR-DECL-BAD-SHAPE)

(check-equal? (parse-decl '(x))

              ERR-DECL-BAD-SHAPE)

(check-equal? (parse-stmt '(a = 1))

              (assignment 'a 1))

(check-equal? (parse-stmt '(if0 1 (a = 1) (a = 1)))

              (if0 1 (parse-block '(a = 1)) (parse-block '(a = 1))))

(check-equal? (parse-stmt '(while0 1 (a = 1)))

              (while0 1 (parse-block '(a = 1))))

(check-equal? (parse-stmt '(x --> f = 1))

              (field-assign 'x 'f 1))

(check-equal? (parse-stmt 1)

              ERR-STATEMENT-BAD-SHAPE)

(check-equal? (parse-block '(block (def x 1)
                                   (def y 2)
                                   (x = 3)))

              (prog (list (parse-decl '(def x 1))
                          (parse-decl '(def y 2)))
                    (list (parse-stmt '(x = 3)))
                    ($)))

(check-equal? (parse-block '(block (a = 1) (b = 2)))

              (prog '()
                    (list (parse-stmt '(a = 1))
                          (parse-stmt '(b = 2)))
                    ($)))

(check-equal? (parse-block '(block))

              ERR-BLOCK-EMPTY)

(check-equal? (parse-block '(block (def x 1)
                                   (def y 2)))

              ERR-BLOCK-EMPTY)

(check-equal? (parse-block '(a = 1))

              (parse-stmt '(a = 1)))

(check-equal? (parse-exp 0)

              0)

(check-equal? (parse-exp 'abc)

              'abc)

(check-equal? (parse-exp '(hello + hi))

              (add 'hello 'hi))

(check-equal? (parse-exp '(hello / hi))

              (div 'hello 'hi))

(check-equal? (parse-exp '(hello == hi))

              (equality 'hello 'hi))

(check-equal? (parse-exp '(hello - hi))

              ERR-EXP-BAD-SHAPE)

(check-equal? (parse-exp '(new c (varOne varTwo)))

              (new 'c '(varOne varTwo)))

(check-equal? (parse-exp '(new c ()))

              (new 'c '()))

(check-equal? (parse-exp '(var --> fieldOne))

              (field-access 'var 'fieldOne))

(check-equal? (parse-exp '(var --> methodName (varOne varTwo)))

              (method-call 'var 'methodName '(varOne varTwo)))

(check-equal? (parse-exp '(var isa c))

              (isa 'var 'c))

(check-equal? (parse-number -1000.1)

              ERR-NUMBER-TOO-SMALL)

(check-equal? (parse-number 1000.1)

              ERR-NUMBER-TOO-BIG)

(check-equal? (parse-number -999.9)

              -999.9)

(check-equal? (parse-number 999.9)

              999.9)

(check-equal? (parse-variable 'xv6)

              ERR-NOT-ALPHABETIC)

(check-equal? (parse-variable 'aaaaaaaaaaaaaaaaaaaatwentyone)

              ERR-NAME-LENGTH-OVER-TWENTY)

(check-equal? (parse-variable 'if0)

              ERR-VARIABLE-IS-KEYWORD)

(check-equal? (parse-variable '==)

              ERR-VARIABLE-IS-KEYWORD)

(check-equal? (parse-variable "hello")

              ERR-VARIABLE-NOT-SYMBOL)

(check-equal? (parse-variable 4)

              ERR-VARIABLE-NOT-SYMBOL)

(check-equal? (parse-variable 'a)

              'a)

(check-equal? (parse-variable 'bc)

              'bc)

(check-pred has-errors?

            (parse-system '((tmodule mOne
                                     (class goodClass (fieldOne)
                                       (method module (paramOne) 1.0))
                                     (((fieldOne Number)) ((module ((() ())) Number))))
                            (import mOne)
                            (def x 1.0)
                            (x = (new goodClass (x)))
                            (x --> module (x)))))

(check-pred (negate has-errors?)

            (parse-system '((tmodule Mone (class cone () (method m (x) x)) (() ((m (Number) Number))))
                            (tmodule
                             Mtwo
                             (timport
                              Mone
                              (() ((m ((() ((m (Number) Number)))) (() ((m (Number) Number)))))))
                             (class ctwo () (method m (x) x))
                             (() ((m (Number) Number))))
                            (timport Mone (() ((m (Number) Number))))
                            (def mone (new Mone ()))
                            (def two 2.0)
                            (mone --> m (two)))))