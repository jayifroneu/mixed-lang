#lang racket

(require "../lib/ast.rkt")
(require "../lib/parse.rkt")
(require "../lib/type.rkt")
(require rackunit)

(define SYSTEM-1 (parse-system '((tmodule Point
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
                                 x)))

(check-pred (negate type-valid?)

              SYSTEM-1)

(check-pred (negate type-valid?/modules)

              (system-mmodule^* SYSTEM-1))

(check-pred (negate (λ (mmodule-in) (type-valid?/module mmodule-in (take (system-mmodule^* SYSTEM-1) 1))))

              (list-ref (system-mmodule^* SYSTEM-1) 1))