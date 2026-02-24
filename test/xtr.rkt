#lang racket

(require "../lib/xtr.rkt")
(require rackunit)

#;(check-equal? (xmodule '((module mOne
                             (class goodClass ()))
                           (module mTwo (import mOne)
                             (class goodClass ()))
                           (import mOne)
                           (import mTwo)
                           (def x 1.0)
                           (x = 2.0)
                           (new goodClass ())))

                "object")

#;(check-equal? (xmodule '((module Point (class Point
                                           (x y)
                                           (method delta (x) (def y (this --> y)) (x = 1.0) (x + y))))
                           (import Point)
                           (def x 1.0)
                           (def point (new Point (x x)))
                           (point --> x = x)
                           (x = (point --> delta (x)))
                           x))

                2.0)

#;(check-equal? (xmodule '((module Point (class Point
                                           (x y)
                                           (method delta (x) (def y (this --> y)) (x = 1.0) (x + y))))
                           (module Point (class Point
                                           (x y z)
                                           (method
                                            delta
                                            ()
                                            (def x (this --> x))
                                            (def y (this --> y))
                                            (x + y))))
                           (import Point)
                           (def x 1.0)
                           (def point (new Point (x x)))
                           (point --> x = x)
                           (x = (point --> delta (x)))
                           x))

                "duplicate module name")

#;(check-equal? (xmodule '((module PointThreeD (class PointThreeD
                                                 (x y z)
                                                 (method
                                                  delta
                                                  ()
                                                  (def x (this --> x))
                                                  (def y (this --> y))
                                                  (x + y))))
                           (import PointThreeD)
                           (def x 1.0)
                           (def point (new Point (x x)))
                           (point --> x = x)
                           (x = (point --> delta (x)))
                           x)
                         )

                "undeclared variable error")

#;(check-equal? (xmodule '((module M (class C () (method val () 17.0)))
                           (module N (class C
                                       ()
                                       (method val () 39.0)
                                       (method go () (def x (new C ())) (x --> val ()))))
                           (import M)
                           (def y (new C ()))
                           (y --> go ())))

                "run-time error")

#;(check-equal? (xmodule '((module M (class C
                                       ()
                                       (method val () 49.0)
                                       (method go () (def x (new C ())) (x --> val ()))))
                           (module N (class C
                                       ()
                                       (method val () 39.0)
                                       (method go () (def x (new C ())) (x --> val ()))))
                           (import M)
                           (def y (new C ()))
                           (y --> go ())))

                49.0)

#;(check-equal? (xmodule '((module C (class C
                                       (f)
                                       (method
                                        eq
                                        (other)
                                        (def old (this --> f))
                                        (def myf old)
                                        (def urf (other --> f))
                                        (def res 1.0)
                                        (if0
                                         (old isa C)
                                         (this --> f = 42.0)
                                         (this --> f = (old + res)))
                                        (urf = (other --> f))
                                        (myf = (this --> f))
                                        (res = (urf == myf))
                                        (this --> f = old)
                                        res)))
                           (import C)
                           (def one 1.0)
                           (def c (new C (one)))
                           (def d c)
                           (def e (new C (c)))
                           (one = (c --> eq (d)))
                           (d = (c --> eq (e)))
                           (one + d)))

                1.0)

#;{(check-equal? (xlink '((tmodule mOne
                                   (class classA (fieldOne fieldTwo))
                                   (((fieldOne Number) (fieldTwo Number)) ()))
                          (tmodule mOne
                                   (import mOne)
                                   (class classA (fieldOne))
                                   (((fieldOne Number)) ()))
                          1.0))

                 "duplicate module name")

   (check-equal? (xlink '((tmodule
                           Point
                           (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
                           (((x Number) (y Number)) ((delta (Number) Number))))
                          (import Point)
                          (def x 1.0)
                          (def point (new Point (x x)))
                          (point --> x = x)
                          (x = (point --> delta (x)))
                          x))

                 '|["Point"]|)

   (check-equal? (xlink '((tmodule Point
                                   (class Point (x y)
                                     (method delta (x)
                                             (def y (this --> y))
                                             (x = 1.0)
                                             (x + y)))
                                   (((x Number) (y Number))
                                    ((delta (Number) Number))))
                          (tmodule PointTwo
                                   (class Point (x y z)
                                     (method delta ()
                                             (def x (this --> x))
                                             (def y (this --> y))
                                             (x + y)))
                                   (((x Number) (y Number))
                                    ((delta (Number) (((x Number) (y Number)) ((delta (Number) Number)))))))
                          (import Point)
                          (def x 1.0)
                          (def point (new Point (x x)))
                          (point --> x = x)
                          (x = (point --> delta (x)))
                          x))

                 "type error")

   (check-equal? (xlink '((tmodule
                           PointThreeD
                           (class PointThreeD
                             (x y z)
                             (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
                           (((x Number) (z Number) (y Number)) ((delta (Number) Number))))
                          (import PointThreeD)
                          (def x 1.0)
                          (def point (new Point (x x)))
                          (point --> x = x)
                          (x = (point --> delta (x)))
                          x))

                 "undeclared variable error")

   (check-equal? (xlink '((tmodule Mone (class cone () (method m (x) x)) (() ((m (Number) Number))))
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
                          (mone --> m (two))))

                 "bad import")

   (check-equal? (xlink '((module Mone
                            (class cone ()
                              (method m (x) x)))
                          (tmodule Mtwo
                                   (timport Mone (() ((m (Number) Number))))
                                   (class ctwo ()
                                     (method m (x) x))
                                   (() ((m ((() ((m (Number) Number)))) (() ((m (Number) Number)))))))
                          (timport Mone (() ((m (Number) Number))))
                          (import Mtwo)
                          (def mone (new cone ()))
                          (def mtwo (new ctwo ()))
                          (def monetoo (mtwo --> m (mone)))
                          (def two 2.0)
                          (monetoo --> m (two))))

                 '|["Mone.into.Body", "Mone.into.Mtwo", "Mone", "Mtwo"]|)

   (check-equal? (xlink '((tmodule
                           Counter
                           (class Counter (count) (method getCount () (this --> count)))
                           (((count Number)) ((getCount () Number))))
                          (import Counter)
                          (def u 3.0)
                          (def w 42.0)
                          (def v w)
                          (def d (new Counter (w)))
                          (while0 (w == v) (block (def c (new Counter (u))) (v = (w + v)) (d = c)))
                          (d --> getCount ())))

                 '|["Counter"]|)

   (check-equal? (xlink '((tmodule
                           Cowboy
                           (class Cowboy () (method draw () 1.0))
                           (() ((draw () Number))))
                          (module Artist (class Artist () (method draw () 666.0)))
                          (import Cowboy)
                          (timport Artist (() ((draw () Number))))
                          (def a (new Artist ()))
                          (def c (new Cowboy ()))
                          (def what 1.0)
                          (def x (new Artist ()))
                          (if0 what (x = a) (x = c))
                          (x --> draw ())))

                 '|["Artist.into.Body", "Cowboy", "Artist"]|)

   (check-equal? (xlink '((module Mone (class cone () (method m (x) x)))
                          (tmodule
                           Mtwo
                           (timport Mone (() ((m (Number) Number))))
                           (class ctwo () (method m (x) x))
                           (() ((m ((() ((m (Number) Number)))) (() ((m (Number) Number)))))))
                          (timport Mone (() ((m (Number) Number))))
                          (import Mtwo)
                          (def mone (new cone ()))
                          (def mtwo (new ctwo ()))
                          (def monetoo (mtwo --> m (mone)))
                          (def two 2.0)
                          (monetoo --> m (two))))

                 '|["Mone.into.Body", "Mone.into.Mtwo", "Mone", "Mtwo"]|)}

(check-equal? (xtr '((tmodule
                      Point
                      (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
                      (((x Number) (y Number)) ((delta (Number) Number))))
                     (import Point)
                     (def x 1.0)
                     (def point (new Point (x x)))
                     (point --> x = x)
                     (x = (point --> delta (x)))
                     x))

              2.0)

(check-equal? (xtr '((module Point (class Point
                                     (x y)
                                     (method delta (x) (def y (this --> y)) (x = 1.0) (x + y))))
                     (tmodule
                      PointTwo
                      (timport Point (((x Number) (y Number)) ((delta (Number) Number))))
                      (timport
                       Point
                       (((x Number) (y Number))
                        ((delta ((((x Number) (y Number)) ((delta (Number) Number)))) Number))))
                      (class Point
                        (x y z)
                        (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
                      (((x Number) (y Number))
                       ((delta (Number) (((x Number) (y Number)) ((delta (Number) Number)))))))
                     (timport Point (((x Number) (y Number)) ((delta (Number) Number))))
                     (def x 1.0)
                     (def point (new Point (x x)))
                     (point --> x = x)
                     (x = (point --> delta (x)))
                     x))

              "bad import")

(check-equal? (xtr '((tmodule
                      PointThreeD
                      (class PointThreeD
                        (x y z)
                        (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
                      (((x Number) (z Number) (y Number)) ((delta (Number) Number))))
                     (import PointThreeD)
                     (def x 1.0)
                     (def point (new Point (x x)))
                     (point --> x = x)
                     (x = (point --> delta (x)))
                     x))

              "undeclared variable error")

(check-equal? (xtr '((tmodule TMod
                              (class TClass
                                (f g)
                                (method m (h)
                                        (def G (this --> g))
                                        (def F (this --> f))
                                        (def FplusG (F + G))
                                        (FplusG + h)))
                              (((f Number) (g Number)) ((m (Number) Number))))
                     (import TMod)
                     (def F 1.0)
                     (def G 2.0)
                     (def t (new TClass (F G)))
                     (def H 7.0)
                     (t --> g = 3.0)
                     (t --> m (H))))

              11.0)

(check-equal? (xtr '((module M
                       (class C ()
                         (method m ()
                                 (new C ()))))
                     (timport M (() ((m () Number))))
                     (def c (new C ()))
                     (c --> m ())))

              "run-time error")