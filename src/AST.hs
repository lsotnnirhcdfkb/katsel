module AST where

import Location

{-
    most asts are just a plain adt, but some asts are required in multiple asts
    for example, function declarations are needed in a standard Decl and also in an ImplMember
    to prevent duplication, these asts are separated into separate datatypes

    so, a Decl is a variant, but a FunDecl is a separated datatype (due to its multiple uses)

    so, there are 2 types of datatypes in this module:
        - variant datatypes
        - separated datatypes

    to prevent name collisions, variant datatypes are prefixed with 'D', and separated datatypes are prefixed with 'S'
    constructor names are prefixed with the datatype name and a '''
    so, the Decl/FunctionDecl hierarchy would be

        data DDecl
            = DDecl'Fun SFunDecl
            ...

        data SFunDecl
            = SFunDecl' DType ...
-}

type LocStr = Located String

data Mutability = Mutable | Immutable
data ThisParamKind = Value | Ref | MutRef

data BinOp
    = Plus | Minus | Star | Slash | Percent
    | Greater | Less | GreaterEqual | LessEqual
    | Amper | Pipe | Caret
    | DoubleGreater | DoubleLess
    | DoubleEqual | BangEqual

data ShortOp = DoubleAmper | DoublePipe
data UnaryOp = UnBang | UnTilde | UnMinus | UnStar
data AssignOp = Equal

data SFunDecl = SFunDecl' DType LocStr [DParam] SBlockExpr

data SBlockExpr = SBlockExpr' [DStmt]

data DCU = DCU'CU [DDecl]

data DDecl
    = DDecl'Fun SFunDecl
    | DDecl'Impl DType [DImplMember]

data DImplMember
    = DImplMember'Fun SFunDecl

data DStmt
    = DStmt'Var DType Mutability LocStr (Maybe (Span, DExpr))
    | DStmt'Expr DExpr
    | DStmt'Ret DExpr

data DExpr
    = DExpr'Block SBlockExpr
    | DExpr'If Span DExpr DExpr (Maybe (Span, DExpr))
    | DExpr'While DExpr DExpr
    | DExpr'Assign DExpr AssignOp DExpr
    | DExpr'ShortCircuit DExpr ShortOp DExpr
    | DExpr'Binary DExpr BinOp DExpr
    | DExpr'Cast DType DExpr
    | DExpr'Unary UnaryOp DExpr
    | DExpr'Ref Span Mutability DExpr
    | DExpr'Call DExpr Span [DExpr]
    | DExpr'Field DExpr Span LocStr
    | DExpr'Method DExpr Span LocStr Span [DExpr]
    | DExpr'Bool Bool
    | DExpr'Float Double
    | DExpr'Int Integer
    | DExpr'Char Char
    | DExpr'String String
    | DExpr'This
    | DExpr'Path DPath

data DParam
    = DParam'Normal Mutability DType LocStr
    | DParam'This ThisParamKind

data DType
    = DType'Path DPath
    | DType'Pointer Mutability DType
    | DType'This

data DPath = DPath' [LocStr]
