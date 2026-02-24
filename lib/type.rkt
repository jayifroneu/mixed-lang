#lang racket

(require "ast.rkt")
(require "valid.rkt")

(provide (all-defined-out))

;; A Shapes ("SClasses") is a [Hash-of ClassName -> Shape], and represents a mapping from the names of
;; classes to their shapes.
(define EMPTY-SHAPES (hash))

;; lookup/shape : ClassName, Shapes -> Shape
;; The Shape associated with the consumed ClassName in the consumed Shapes.
(define (lookup/shape c shapes)
  (hash-ref shapes c))

;; update/shape : ClassName, Shape, Shapes -> Shapes
;; Updates the consumed Shapes to associated the consumed ClassName with the consumed Shape.
(define (update/shape c shape shapes)
  (hash-set shapes c shape))

;; A TVar is a [Hash-of Variable -> Type], and represents a mapping of variables to their types.
(define EMPTY-TVAR (hash))

;; lookup/type : Variable, TVar -> Type
;; The Type of the consumed Variable in the consumed TVar.
(define (lookup/type x tvar)
  (hash-ref tvar x))

;; update/type : Variable, Type, TVar -> TVar
;; Updates the consumed TVar to associated the consumed Variable with the consumed Type.
(define (update/type x t tvar)
  (hash-set tvar x t))

;; lookup/field-type -> FieldName, Shape -> [Maybe FieldType]
;; Produces the FieldType in the consumed Shape that has the consumed FieldName, or #f if no such
;; FieldType exists.
(define (lookup/field-type f shape)
  (findf (λ (ft)
           (symbol=? (field-type-name ft) f))
         (shape-field-type^* shape)))

;; lookup/method-type -> MethodName, Shape -> [Maybe MethodType]
;; Produces the MethodType in the consumed Shape that has the consumed MethodName, or #f if no such
;; MethodType exists.
(define (lookup/method-type m shape)
  (findf (λ (mt)
           (symbol=? (method-type-name mt) m))
         (shape-method-type^* shape)))

;; type-valid? : System -> Boolean
;; Whether the consumed System is type-valid.
(define (type-valid? system-in)
  (match system-in
    [(system mmodule^* mimport^* prog)
     ;; create the Shapes for the body of the system, given its imports
     #:do [(define shapes (import^*->shapes mimport^* mmodule^*))]
     ;; a system is type-valid if ... 
     (and #;{... the modules it declares are type-valid}
          (type-valid?/modules mmodule^*)
          #;{... and its body is type-valid and produces a number}
          (type-valid?/program prog 'number shapes EMPTY-TVAR))]))


;; import^*->shapes : [List-of Import], [List-of TModule] -> Shapes
;; Produces the shapes that the consumed Imports make available, given that they occur in the scope of
;; the consumed Modules.
(define (import^*->shapes mimport^*-in mmodule^*-in)
  (make-immutable-hash #;{map each import to the shape of the module it refers to}
                       (map (λ (mimport-in)
                              (let ([mmodule-in (lookup/module (import-name mimport-in)
                                                               mmodule^*-in)])

                                (cons (class-name (module-class mmodule-in))
                                      (cond [(typed? mimport-in)
                                             (import-shape mimport-in)]
                                            [(not (typed? mimport-in))
                                             (class-shape (module-class mmodule-in))]))))
                            mimport^*-in)))

;; type-valid?/modules : [List-of TModule] -> Boolean
;; Whether the consumed sequence of modules is type-valid.
(define (type-valid?/modules mmodule^*-in)
  ;; an accumulator is needed to keep track of previous modules relative to the current module,
  ;; because it may import them and therefore use the shapes they export
  (define (type-valid?/modules/accm mmodule^*-in prev-mmodule^*)
    (match mmodule^*-in
      ;; if there are no modules left to examine, they are trivially type-valid
      ['()                   #t]
      ;; otherwise...
      [(cons mmodule-in rst) (and #;{... check that the current module is type-valid given the
                                         previous modules}
                                  (type-valid?/module mmodule-in prev-mmodule^*)
                                  #;{... check that the rest of the modules are type-valid, adding the
                                         current module to the accumulated previous modules}
                                  (type-valid?/modules/accm rst (cons mmodule-in prev-mmodule^*)))]))

  ;; initially, the list of previous modules must be empty
  (type-valid?/modules/accm mmodule^*-in '()))

;; type-valid?/module : TModule, [List-of TModule] -> Boolean
;; Whether the consumed Module is type-valid given a sequence of previously declared Modules that it
;; may import.
(define (type-valid?/module mmodule-in prev-mmodule^*)
  (match-define (module _ mimport^*-in class-in) mmodule-in)
  
  (or (not (typed? mmodule-in))
      (type-valid?/class class-in
                         (update/shape (class-name class-in)
                                       (class-shape class-in)
                                       (import^*->shapes mimport^*-in prev-mmodule^*)))))

;; type-valid?/class : Class, Shapes -> Boolean
;; Whether the consumed Class is type-valid, given the shapes of the classes it may reference.
(define (type-valid?/class class-in shapes)
  ;; get the shape of this class
  (define shape (lookup/shape (class-name class-in) shapes))
  ;; sort the class methods alphabetically, as they may appear in any order relative to this class's
  ;; shape
  (define method^*-sorted (sort (class-method^* class-in) symbol<? #:key method-name))
  ;; sort the method types alphabetically, as they may appear in any order relative to this shape's
  ;; class
  (define mt^*-sorted (sort (shape-method-type^* shape) symbol<? #:key method-type-name))

  ;; a class is type-valid if...
  (and #;{...the fields of the class match up with the fields of its shape by name}
       (list=? (class-field^* class-in)
               (shape-field-type^* shape)
               (λ (f ft) (symbol=? f (field-type-name ft))))
       #;{...the methods of the class match up with the methods of the shape by name}
       (list=? method^*-sorted
               mt^*-sorted
               (λ (m mt) (symbol=? (method-name m) (method-type-name mt))))
       #;{... each method of the class matches with its associated method type in this class's shape
              Additionally, make sure that the method knows that the type of the variable "this" is
              the shape of this class}
       (andmap (λ (m mt) (type-valid?/method m mt shapes (hash 'this shape)))
               method^*-sorted
               mt^*-sorted)))

;; type-valid?/method : Method, MethodType, Shapes, TVar -> Boolean
;; Whether the consumed Method is type-valid and consistent with the consumed MethodType.
(define (type-valid?/method method-in type shapes tvar)
  ;; get the parameters of the method
  (define param^* (method-param^* method-in))
  ;; get the types of those parameters
  (define param-type^*(method-type-param-type^* type))
  ;; a method is type valid if...
  (and #;{...it has the same number of parameters as its MethodType specifies}
       (= (length param^*)
          (length param-type^*))
       #;{...its body is type-valid and produces the type that its MethodType specifies}
       (type-valid?/program (method-body method-in) (method-type-type type) shapes
                            ;; make sure to let the body know the types of the parameters it may
                            ;; reference
                            (foldr (λ (param param-type tvar)
                                     (update/type param param-type tvar))
                                   tvar
                                   param^*
                                   param-type^*))))


;; type-valid?/program : Program, Type, Shapes, TVar
;; Whether the consumed Program is type-valid and produces the consumed Type.
(define (type-valid?/program prog-in type shapes tvar)
  (match prog-in
    [(prog def^* stmt^* exp)
     ;; attempt to update the consumed TVar with the program's declarations
     #:do [(define new-tvar (type-valid?/declarations def^* shapes tvar))]
     ;; continue only if the produced [Maybe TVar] is a TVar
     #:when new-tvar
     ;; a program is type-valid if...
     (and #;{...its statements are type-valid}
          (type-valid?/statements stmt^* shapes new-tvar)
          #;{...its return expression is type-valid and produces the type we expect}
          (type-valid?/expression exp type shapes new-tvar))]
    ;; in any other case (when the declarations are not type-valid), the entire program is
    ;; type-invalid
    [_ #f]))


;; type-valid?/declarations : [List-of Declaration], Shapes, TVar -> [Maybe TVar]
;; Produces an updated version of the consumed TVar using the consumed sequence of Declarations if
;; they are type-valid. Produces #f if not.
(define (type-valid?/declarations def^*-in shapes tvar)
  (match def^*-in
    ;; if the sequence is empty, return the same tvar
    ['() tvar]
    ;; if the sequence contains elements...
    [(cons (def x exp) rst)
     ;; ...attempt to define the type of the variable declared ...
     #:do [(define type (type-of exp shapes tvar))]
     ;; ...if it succeeds (again, [Maybe Type]) ...
     #:when type
     ;; ... continue updating the consumed TVar
     (type-valid?/declarations rst shapes (hash-set tvar x type))]
    ;; if at any point an invalid declaration is encountered, return #f
    [_   #f]))

;; type-valid?/statements : [List-of Statement], Shapes, TVar, Boolean
;; Whether the consumed sequence of Statements is type-valid.
(define (type-valid?/statements stmt^*-in shapes tvar)
  (andmap (λ (stmt) (type-valid?/statement stmt shapes tvar))
          stmt^*-in))

;; type-valid?/statement : Statement, Shapes, TVar -> Boolean
;; Whether the consumed Statement is type-valid.
(define (type-valid?/statement stmt-in shapes tvar)
  (match stmt-in
    ;; an assignment is valid if its expression is type-valid and produces the same type as the
    ;; variable it is being assigned to
    [(assignment x exp) (type-valid?/expression exp (lookup/type x tvar) shapes tvar)]
    ;; a conditional or loop is valid if its test expression is type-valid and any blocks it contains
    ;; are also type valid
    [(if0 tst thn els)
     #:when (type-of tst shapes tvar)
     (and (type-valid?/block thn shapes tvar)
          (type-valid?/block els shapes tvar))]
    [(while0 tst bdy)
     #:when (type-of tst shapes tvar)
     (type-valid?/block bdy shapes tvar)]
    [(field-assign o f exp)
     ;; get the shape of o
     #:do [(define shape (lookup/type o tvar))]
     ;; attempt to find the FieldType of the field being assigned
     #:do [(define ft (lookup/field-type f shape))]
     ;; if you succeed, check that the expression is type valid and produces the same type as the
     ;; field
     #:when ft
     (type-valid?/expression exp (field-type-type ft) shapes tvar)]
    ;; if any of the when clauses failed, the statement is not type-valid
    [_ #f]))

;; type-valid?/block : Block, Shapes, TVar -> Boolean
;; Whether the consumed block is type-valid.
(define (type-valid?/block block-in shapes tvar)
  (match block-in
    [(prog def^* stmt^* ($))
     ;; A little bit of cheating here. Because there is no return expression to check, but we'd still
     ;; like to reuse the function for type-checking programs, we replace the $ marker of the block
     ;; with 0.0 and call that function expecting a number. Of course, that part is now guaranteed to
     ;; pass, and the function effectively only checks the type validity of the declarations and
     ;; statements, which is what we want.
     (type-valid?/program (prog def^* stmt^* 0.0) 'number shapes tvar)]
    ;; otherwise, the block is a single statement
    [_ (type-valid?/statement block-in shapes tvar)]))

;; type-valid?/expression : Expression, Type, Shapes, TVar -> Boolean
;; Whether the consumed Expression is type-valid and produces the consumed Type.
(define (type-valid?/expression exp-in type shapes tvar)
  ;; get the return type of the expression
  (define return (type-of exp-in shapes tvar))

  ;; the expression is type valid if the return is not #f ([Maybe]) and the return matches the
  ;; expected type
  (and return
       (type=? return type)))

;; type-of : Expression, Shapes, TVar -> [Maybe Type]
;; Produces the return Type of the consumed expression, given Shapes and TVar. Produces #f if the
;; consumed expression is itself type-invalid.
(define (type-of exp-in shapes tvar)
  (match exp-in
    ;; a numeric literal is a number
    [(? number?)        'number]
    ;; if the expression is a variable, look it up
    [(? symbol? exp-in) (lookup/type exp-in tvar)]
    ;; an add is valid if both of its arguments are numbers. it produces a number
    [(add v1 v2)
     #:when (type=? 'number (lookup/type v1 tvar))
     #:when (type=? 'number (lookup/type v2 tvar))
     'number]
    ;; same for div
    [(div v1 v2)
     #:when (type=? 'number (lookup/type v1 tvar))
     #:when (type=? 'number (lookup/type v2 tvar))
     'number]
    ;; an equality is always valid and produces a number
    [(equality v1 v2)   'number]
    [(new name a^*)
     ;; get the shape of the class the new is attempting to construct an instance of 
     #:do [(define shape (lookup/shape name shapes))]
     ;; get the FieldTypes of that shape
     #:do [(define ft^* (shape-field-type^* shape))]
     ;; get the types of the arguments
     #:do [(define a-type^* (map (λ (a) (lookup/type a tvar)) a^*))]
     ;; if the types of the arguments match the types in the shape...
     #:when (list=? (map field-type-type ft^*)
                    a-type^*
                    type=?)
     ;; ...then the type of the new expression is that shape
     shape]
    ;; an isa is valid if v is any class. it produces a number
    [(isa v name)
     #:when (shape? (lookup/type v tvar))
     'number]
    ;; similar pattern as before. attempt to get the FieldType of the accessed field, if successful,
    ;; then the expression produces the Type within that FieldType
    [(field-access o f)
     #:do [(define type (lookup/type o tvar))]
     #:when (shape? type)
     #:do [(define ft (lookup/field-type f type))]
     #:when ft
     (field-type-type ft)]
    ;; same as above, but searching through MethodTypes
    ;; if successful, additionally checks that the arguments provided are of the same type as the
    ;; parameters of the method
    [(method-call o name a^*)
     #:do [(define type (lookup/type o tvar))]
     #:when (shape? type)
     #:do [(define mt (lookup/method-type name type))]
     #:when mt
     #:do [(define a-type^* (map (λ (a) (lookup/type a tvar)) a^*))]
     #:do [(define param-type^* (method-type-param-type^* mt))]
     #:when (list=? param-type^*
                    a-type^*
                    type=?)
     (method-type-type mt)]
    ;; if any of the when clauses failed, the expression is not type valid
    [_ #f]))


