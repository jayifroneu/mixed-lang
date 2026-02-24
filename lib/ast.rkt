#lang racket

(require (only-in lang/htdp-intermediate-lambda explode))

(provide (all-defined-out))
(provide explode)

(define KEYWORDS '(if0
                   while0
                   block
                   =
                   /
                   +
                   ==
                   def
                   class
                   method
                   isa
                   new
                   -->
                   module
                   tmodule
                   import
                   timport))

(define ALLOWED-CHARACTERS (append (explode "qwertyuiopasdfghjklzxcvbnm")
                                   (explode "QWERTYUIOPASDFGHJKLZXCVBNM")))

;; An AST is a (system [List-of MixedModule] [List-of MixedImport] Program) and represents the
;; abstract syntax tree of a "Mixed" program with a list of typed and untyped module declarations
;; followed by a list of typed and untyped imports followed by a program.
(struct system [mmodule^* mimport^* prog] #:transparent)

;; A MixedModule is one of
;; - Module
;; - TModule

;; A Module is a (module ModuleName [List-of Import] Class)
(struct module [name import^* class] #:transparent)

;; A TModule is a (module ModuleName [List-of MixedImport] TClass) and represents a
;; module that imports some number of other modules and exports a single class and its shape.

;; A ModuleName is a symbol that is a part of the set of Variables

;; A MixedImport is one of
;; - Import
;; - TImport

;; An Import is a (import ModuleName #f) and represents the import of one module with a specfic name
(struct import [name shape] #:transparent)

;; A TImport is a (import ModuleName Shape) and represents the import of one module with a specfic
;; name

;; typed? : AST -> Boolean
(define (typed? node)
  (match node
    [(import name (? shape?))                                            #t]
    [(module m-name import^* (class c-name field^* method^* (? shape?))) #t]
    [_ #f]))

;; A Class is a (class ClassName [List-of Field] [List-of Method] #f) and represents a class
;; declaration with a name, a list of fields, and a list of methods.
(struct class [name field^* method^* shape] #:transparent)

;; A TClass is a (class ClassName [List-of Field] [List-of Method] Shape) and represents a class
;; declaration with a name, a list of fields, a list of methods, and a Shape.

;; A ClassName is a symbol that is a part of the set of Variables and represents the name of a class.

;; A Field is a symbol that is a part of the set of Variables and represents the name of a field of a
;; class.

;; A Method is a (method MethodName [List-of Parameter] Program) and represents a method of some
;; class with a name, a list of parameters, followed by a block of code that represents the method
;; body.
(struct method [name param^* body] #:transparent)

;; A MethodName is a symbol that is a part of the set of Variables and represents the name of a
;; method.

;; A Parameter is a symbol that is a part of the set of Variables and represents the name of a
;; parameter of a method.

;; A Type is one of
;; - 'number 
;; - Shape

(define (type? v)
  (or (and (symbol? v) (symbol=? v 'number))
      (shape? v)))

;; A Shape is a (shape [List-of FieldType] [List-of MethodType])
(define-struct shape [field-type^* method-type^*] #:transparent)

;; A FieldType is a (field-type FieldName Type)
(define-struct field-type [name type] #:transparent)

;; A MethodType is a (method-type MethodName [List-of Type] Type)
(define-struct method-type [name param-type^* type] #:transparent)

;; A Program is a (prog [List-of Declaration] [List-of Statement] Return) and represents a
;; a block of code with a list of variable declarations followed by a list of statements followed by a
;; final expression.
(struct prog [decl^* stmt^* exp] #:transparent)

;; A Declaration is a (def Variable Expression) and represents the declaration of a value of a
;; variable to some expression.
(struct def [name exp] #:transparent)

;; A Variable is a symbol containing no more than twenty of US keyboard letters, minus the set of
;; keywords.

;; A Return is one of
;; - Expression
;; - (dollar)

;; An Expression is one of
;; - Variable
;; - Number
;; - (add Variable Variable) (representing an addition operation)
(struct add [lhs rhs] #:transparent)
;; - (div Variable Variable) (representing a division operation)
(struct div [lhs rhs] #:transparent)
;; - (equality Variable Variable) (representing a numerical comparison)
(struct equality [lhs rhs] #:transparent)
;; - (new ClassName [List-of Variable]) (representing the instantiation of a class)
(struct new [name var^*] #:transparent)
;; - (field-access Variable Field) (representing accessing some field of some variable)
(struct field-access [var field] #:transparent)
;; - (method-call Variable MethodName [List-of Variable]) (representing a call to some method of
;; some variable with some variables as arguments)
(struct method-call [var name arg^*] #:transparent)
;; - (isa Variable ClassName) (representing a class predicate on some variable)
(struct isa [var name] #:transparent)

;; A Statement is one of
;; - (assignment Variable Expression) (representing the assignment of the value of some variable
;; to the value of some expression)
(struct assignment [name exp] #:transparent)
;; - (if0 Expression Block Block) (representing a conditional whose then-clause runs if its
;; expression evaluates to zero, otherwise running the then-clause)
(struct if0 [exp block1 block2] #:transparent)
;; - (while0 Expression Block) (representing a while statement whose body runs as long as its
;; expression evaluates to zero.
(struct while0 [exp block] #:transparent)
;; - (field-assign Variable Field Expression) (representing assigning the value of some field of
;; some variable to the result of some expression.
(struct field-assign [var field exp] #:transparent)

;; a Block is a (prog [List-of Declaration] [List-of Statement] (dollar)) and represents a
;; block of that has the same shape as a Program, but has no return expression. Rather, a Block has a
;; (dollar) in place of the final expression.
(struct $ [] #:transparent)


;; Any of the above AST nodes may also be an Error (E), which is one of
;; - Parser Error
;; - Validity Error
;; An AST+E represents an AST which may contain Errors.
(struct err [msg] #:transparent)

;; a Parser Error (PE) is an instance of one of the following structs, and represents an error that
;; may be placed in the AST as it is created via parsing.
(define ERR-MIXED-MODULE-BAD-SHAPE  (err "not shaped like a mixed module"))
(define ERR-MODULE-BAD-SHAPE  (err "not shaped like an untyped module"))
(define ERR-TYPED-MODULE-BAD-SHAPE  (err "not shaped like a typed module"))
(define ERR-MIXED-IMPORT-BAD-SHAPE (err "not shaped like a mixed import"))
(define ERR-IMPORT-BAD-SHAPE (err "not shaped like an untyped import"))
(define ERR-TYPED-IMPORT-BAD-SHAPE (err "not shaped like a typed import"))
(define ERR-PROG-BAD-SHAPE (err "not shaped like a program"))
(define ERR-SHAPE-BAD-SHAPE (err "not shaped like a shape"))
(define ERR-FIELD-TYPE-BAD-SHAPE (err "not shaped like a field type"))
(define ERR-METHOD-TYPE-BAD-SHAPE (err "not shaped like a method type"))
(define ERR-TYPE-BAD-SHAPE (err "not shaped like a type"))
(define ERR-CLASS-BAD-SHAPE (err "not shaped like a class"))
(define ERR-DECL-BAD-SHAPE (err "not shaped like a declaration"))
(define ERR-EXP-BAD-SHAPE (err "not shaped like an expression"))
(define ERR-STATEMENT-BAD-SHAPE (err "not shaped like a statement"))
(define ERR-METHOD-BAD-SHAPE (err "not shaped like a method"))
(define ERR-BLOCK-EMPTY (err "empty block"))
(define ERR-VARIABLE-NOT-SYMBOL (err "variable is not a symbol"))
(define ERR-VARIABLE-IS-KEYWORD (err "variable is a keyword"))
(define ERR-NUMBER-TOO-BIG (err "number is greater than 1000.0"))
(define ERR-NUMBER-TOO-SMALL (err "number is less than -1000.0"))
(define ERR-DECL-AFTER-STMTS (err "found a declaration after statements"))
(define ERR-NAME-LENGTH-OVER-TWENTY (err "found a variable name of length greater than 20"))
(define ERR-NOT-ALPHABETIC (err "variable is named with non-alphabetic characters"))

;; a Validity Error is one of
;; - Duplicate Module Name (DMNE)
(define ERR-DUPLICATE-MODULE-NAME (err "duplicate module name"))
;; - Duplicate Class Members (DCME)
(define ERR-DUPLICATE-CLASS-MEMBERS (err "duplicate method, field, or parameter name"))
;; - Undeclared Variable (UVE)
(define ERR-UNDECLARED-VARIABLE (err "undeclared variable error"))
;; - Bad Import (BIE)
(define ERR-BAD-IMPORT (err "bad import"))

;; has-errors? : AST+E -> Boolean
;; Whether the consumed AST contains errors.
(define (has-errors? ast)
  (match ast 
    [(? err?)                            #t]
    [(system mmodule^* mimport^* prog)   (or (ormap has-errors? mmodule^*)
                                             (ormap has-errors? mimport^*)
                                             (has-errors? prog))]
    [(module name import^* class)        (or (has-errors? name)
                                             (ormap has-errors? import^*)
                                             (has-errors? class))]
    [(import name shape)                 (or (has-errors? name)
                                             (and shape (has-errors? shape)))]
    [(class name field^* method^* shape) (or (has-errors? name)
                                             (ormap has-errors? field^*)
                                             (ormap has-errors? method^*)
                                             (and shape (has-errors? shape)))]
    [(shape field-type^* method-type^*)  (or (ormap has-errors? field-type^*)
                                             (ormap has-errors? method-type^*))]
    [(field-type name type)              (or (has-errors? name)
                                             (has-errors? type))]
    [(method-type name type^* type)      (or (has-errors? name)
                                             (ormap has-errors? type^*)
                                             (has-errors? type))]
    [(method name param^* body)          (or (has-errors? name)
                                             (ormap has-errors? param^*)
                                             (has-errors? body))]
    [(prog decl^* stmt^* r)              (or (ormap has-errors? stmt^*)
                                             (ormap has-errors? decl^*)
                                             (has-errors? r))]
    [(def v exp)                         (or (has-errors? v)
                                             (has-errors? exp))]
    [(assignment v exp)                  (or (has-errors? v)
                                             (has-errors? exp))]
    [(if0 exp b1 b2)                     (or (has-errors? exp)
                                             (has-errors? b1)
                                             (has-errors? b2))]
    [(while0 exp b)                      (or (has-errors? exp)
                                             (has-errors? b))]
    [(field-assign v field exp)          (or (has-errors? v)
                                             (has-errors? field)
                                             (has-errors? exp))]
    [(add v1 v2)                         (or (has-errors? v1)
                                             (has-errors? v2))]
    [(div v1 v2)                         (or (has-errors? v1)
                                             (has-errors? v2))]
    [(equality v1 v2)                    (or (has-errors? v1)
                                             (has-errors? v2))]
    [(new name v^*)                      (or (has-errors? name)
                                             (ormap has-errors? v^*))]
    [(field-access v field)              (or (has-errors? v)
                                             (has-errors? field))]
    [(method-call v name arg^*)          (or (has-errors? v)
                                             (has-errors? name)
                                             (ormap has-errors? arg^*))]
    [(isa v name)                        (or (has-errors? v)
                                             (has-errors? name))]
    [(? $?)                              #f]
    [(? symbol?)                         #f]
    [(? number?)                         #f]))

;; type=? : Type, Type -> Boolean
;; Whether the two consumed types are equivalent. They are equivalent if they are both the GoodNumber
;; type or if they are both Shapes with the same FieldTypes in the same order and the same MethodTypes
;; in any order.
(define (type=? t1 t2)
  (cond [(and (symbol? t1)
              (symbol? t2)) #t]
        [(and (shape? t1)
              (shape? t2))  (shape=? t1 t2)]
        [else               #f]))

;; shape=? : Shape, Shape -> Boolean
;; Whether the two consumed shapes are equivalent. See type=?.
(define (shape=? s1 s2)
  (and (list=? (shape-field-type^* s1)
               (shape-field-type^* s2)
               field-type=?)
       (list=? (sort (shape-method-type^* s1) symbol<? #:key method-type-name)
               (sort (shape-method-type^* s2) symbol<? #:key method-type-name)
               method-type=?)))

;; list=? : {X} [List-of X], [List-of X], [X, X -> Boolean] -> Boolean
;; Whether the two consumed lists have equal length and the same elements (determined by the consumed
;; function) in the same order.
(define (list=? xs1 xs2 x=?)
  (match/values (values xs1 xs2)
                [('() '())                       #t]
                [((cons x1 rst1) (cons x2 rst2)) (and (x=? x1 x2)
                                                      (list=? rst1 rst2 x=?))]
                [(_ _)                           #f]))

;; field-type=? : FieldType, FieldType -> Boolean
(define (field-type=? ft1 ft2)
  (and (symbol=? (field-type-name ft1)
                 (field-type-name ft2))
       (type=? (field-type-type ft1)
               (field-type-type ft2))))

;; method-type=? : MethodType, MethodType -> Boolean
(define (method-type=? mt1 mt2)
  (and (symbol=? (method-type-name mt1)
                 (method-type-name mt2))
       (list=? (method-type-param-type^* mt1)
               (method-type-param-type^* mt2)
               type=?)
       (type=? (method-type-type mt1)
               (method-type-type mt1))))