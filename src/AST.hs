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

    all the datatypes (except for a few, like 'Mutability' and 'ThisParamKind') when used also should be located, so there are type aliases that are used for located types
    the type alias names are prefixed with 'L', so 'DDecl' would have the type alias:

        type LDDecl = Located Decl

    'BinOp' would have the type alias:

        type LBinOp = Located BinOp

    and 'SFunDecl' would have the type alias:

        type DSFunDecl = Located SFunDecl
-}

type LocStr = Located String

data Mutability = Mutable | Immutable
data ThisParamKind = Value | Ref | MutRef

type LBinOp = Located BinOp
data BinOp
    = Plus | Minus | Star | Slash | Percent
    | Greater | Less | GreaterEqual | LessEqual
    | Amper | Pipe | Caret
    | DoubleGreater | DoubleLess
    | DoubleEqual | BangEqual

type LShortOp = Located ShortOp
data ShortOp = DoubleAmper | DoublePipe
type LUnaryOp = Located UnaryOp
data UnaryOp = UnBang | UnTilde | UnMinus | UnStar
type LAssignOp = Located AssignOp
data AssignOp = Equal

type LSFunDecl = Located SFunDecl
data SFunDecl = SFunDecl' (Maybe LDType) LocStr [LDParam] LSBlockExpr

type LSBlockExpr = Located SBlockExpr
data SBlockExpr = SBlockExpr' [LDStmt]

type LDModule = Located DModule
data DModule = DModule' [LDDecl]

type LDDecl = Located DDecl
data DDecl
    = DDecl'Fun LSFunDecl
    | DDecl'Impl LDType [LDImplMember]

type LDImplMember = Located DImplMember
data DImplMember
    = DImplMember'Fun LSFunDecl

type LDStmt = Located DStmt
data DStmt
    = DStmt'Var LDType Mutability LocStr (Maybe (Span, LDExpr))
    | DStmt'Expr LDExpr
    | DStmt'Ret LDExpr

type LDExpr = Located DExpr
data DExpr
    = DExpr'Block LSBlockExpr
    | DExpr'If Span LDExpr LDExpr (Maybe (Span, LDExpr))
    | DExpr'While LDExpr LDExpr
    | DExpr'Assign LDExpr LAssignOp LDExpr
    | DExpr'ShortCircuit LDExpr LShortOp LDExpr
    | DExpr'Binary LDExpr LBinOp LDExpr
    | DExpr'Cast LDType LDExpr
    | DExpr'Unary LUnaryOp LDExpr
    | DExpr'Ref Span Mutability LDExpr
    | DExpr'Call LDExpr Span [LDExpr]
    | DExpr'Field LDExpr Span LocStr
    | DExpr'Method LDExpr Span LocStr Span [LDExpr]
    | DExpr'Bool Bool
    | DExpr'Float Double
    | DExpr'Int Integer
    | DExpr'Char Char
    | DExpr'String String
    | DExpr'This
    | DExpr'Path LDPath

type LDParam = Located DParam
data DParam
    = DParam'Normal Mutability LDType LocStr
    | DParam'This ThisParamKind

type LDType = Located DType
data DType
    = DType'Path LDPath
    | DType'Pointer Mutability LDType
    | DType'This

type LDPath = Located DPath
data DPath = DPath' [LocStr]
