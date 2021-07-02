module AST.Datatypes 
    ( ExprPrec(..)
    , BinOp(..)
    , ShortOp(..)
    , UnaryOp(..)
    , AssignOp(..)

    , SFunDecl(..)
    , SBlockExpr(..)

    , DModule(..)
    , DDecl(..)
    -- , DImplEntity(..)
    , DStmt(..)
    , DExpr(..)
    , DParam(..)
    , DType(..)
    , DPath(..)

    , LSFunDecl
    , LSBlockExpr

    , LDModule
    , LDDecl
    -- , LDImplEntity
    , LDStmt
    , LDExpr
    , LDParam
    , LDType
    , LDPath

    , prec_of_bin_op
    , prec_of_short_op
    ) where

import Location

{-
    most asts are just a plain adt, but some asts are required in multiple asts
    for example, function declarations are needed in a standard Decl and also in an ImplEntity
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

    all the datatypes (except for a few, like 'Mutability') when used also should be located, so there are type aliases that are used for located types
    the type alias names are prefixed with 'L', so 'DDecl' would have the type alias:

        type LDDecl = Located Decl

    'BinOp' would have the type alias:

        type LBinOp = Located BinOp

    and 'SFunDecl' would have the type alias:

        type DSFunDecl = Located SFunDecl
-}

type LocStr = Located String

data ExprPrec
    = PrecAssign | PrecBinOr | PrecBinAnd
    | PrecCompEQ | PrecCompLGT | PrecBitXor | PrecBitOr
    | PrecBitAnd | PrecBitShift | PrecAdd | PrecMult
    | PrecCast | PrecUnary | PrecCall | PrecPrimary
    deriving (Eq, Ord) -- Ord derives it as the later constructors are greater, i.e. PrecPrimary > PrecCall and PrecCall > PrecBlockLevel

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
data UnaryOp = UnBang | UnTilde | UnMinus
type LAssignOp = Located AssignOp
data AssignOp = Equal

prec_of_bin_op :: BinOp -> ExprPrec
prec_of_bin_op Plus = PrecAdd
prec_of_bin_op Minus = PrecAdd
prec_of_bin_op Star = PrecMult
prec_of_bin_op Slash = PrecMult
prec_of_bin_op Percent = PrecMult
prec_of_bin_op Greater = PrecCompLGT
prec_of_bin_op Less = PrecCompLGT
prec_of_bin_op GreaterEqual = PrecCompLGT
prec_of_bin_op LessEqual = PrecCompLGT
prec_of_bin_op Amper = PrecBitAnd
prec_of_bin_op Pipe = PrecBitOr
prec_of_bin_op Caret = PrecBitXor
prec_of_bin_op DoubleGreater = PrecBitShift
prec_of_bin_op DoubleLess = PrecBitShift
prec_of_bin_op DoubleEqual = PrecCompEQ
prec_of_bin_op BangEqual = PrecCompEQ

prec_of_short_op :: ShortOp -> ExprPrec
prec_of_short_op DoubleAmper = PrecBinAnd
prec_of_short_op DoublePipe = PrecBinOr

type LSFunDecl = Located SFunDecl
data SFunDecl = SFunDecl' (Maybe LDType) LocStr [LDParam] LSBlockExpr

type LSBlockExpr = Located SBlockExpr
data SBlockExpr = SBlockExpr' [LDStmt]

type LDModule = Located DModule
data DModule = DModule' [LDDecl]

type LDDecl = Located DDecl
data DDecl
    = DDecl'Fun LSFunDecl
    {- | DDecl'Impl LDType [LDImplEntity] -}

{-
type LDImplEntity = Located DImplEntity
data DImplEntity
    = DImplEntity'Fun LSFunDecl
-}

type LDStmt = Located DStmt
data DStmt
    = DStmt'Var LDType LocStr (Maybe LDExpr)
    | DStmt'Expr LDExpr

type LDExpr = Located DExpr
data DExpr
    = DExpr'Block LSBlockExpr
    | DExpr'If LDExpr LDExpr (Maybe LDExpr)
    | DExpr'While LDExpr LDExpr
    | DExpr'Assign LDExpr LAssignOp LDExpr
    | DExpr'ShortCircuit LDExpr LShortOp LDExpr
    | DExpr'Binary LDExpr LBinOp LDExpr
    | DExpr'Cast LDType LDExpr
    | DExpr'Unary LUnaryOp LDExpr
    -- | DExpr'Deref LDExpr
    -- | DExpr'Ref LDExpr
    | DExpr'Call LDExpr [LDExpr]
{-
    | DExpr'Field LDExpr LocStr
    | DExpr'Method LDExpr LocStr [LDExpr]
-}
    | DExpr'Bool Bool
    | DExpr'Float Double
    | DExpr'Int Integer
    | DExpr'Char Char
    | DExpr'String String
    {- | DExpr'This -}
    | DExpr'Path LDPath
    | DExpr'Ret LDExpr

type LDParam = Located DParam
data DParam
    = DParam'Normal LDType LocStr

type LDType = Located DType
data DType
    = DType'Path LDPath
    -- | DType'Pointer LDType
    {- | DType'This -}

type LDPath = Located DPath
data DPath = DPath' [LocStr]
