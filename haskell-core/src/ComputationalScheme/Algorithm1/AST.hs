{-# LANGUAGE DerivingStrategies #-}

-- | Abstract Syntax Tree representation for R5RS Scheme
--
-- This module defines the AST data structures for representing parsed
-- Scheme programs. We support a core subset of R5RS without macros initially.
module ComputationalScheme.Algorithm1.AST where

import ComputationalScheme.Types (BindingId)
import Data.Text (Text)
import qualified Data.Text as T

-- | Source location for error reporting
data SourceLoc = SourceLoc
  { locLine :: !Int    -- ^ Line number (1-indexed)
  , locCol  :: !Int    -- ^ Column number (1-indexed)
  , locPos  :: !Int    -- ^ Absolute position in source
  }
  deriving (Eq, Ord, Show)

-- | AST node types for R5RS Scheme
data Expr
  = Const Constant SourceLoc          -- ^ Self-evaluating constants
  | Var Text SourceLoc                -- ^ Variable reference
  | Lambda LambdaForm SourceLoc       -- ^ Lambda abstraction
  | Let LetForm SourceLoc             -- ^ Let binding
  | LetRec LetForm SourceLoc          -- ^ Letrec binding
  | Define DefineForm SourceLoc       -- ^ Top-level definition
  | If IfForm SourceLoc               -- ^ Conditional
  | Cond [CondClause] SourceLoc      -- ^ Multi-way conditional
  | Application Expr [Expr] SourceLoc -- ^ Function application
  | Begin [Expr] SourceLoc            -- ^ Sequence
  | CallCC Expr SourceLoc             -- ^ First-class continuation
  deriving (Eq, Show)

-- | Self-evaluating constants
data Constant
  = Number Double                      -- ^ Numeric literal
  | String Text                        -- ^ String literal
  | Bool Bool                          -- ^ Boolean
  | Char Char                          -- ^ Character
  | EmptyList                          -- ^ Empty list '()
  | Quote Expr                         -- ^ Quoted expression
  deriving (Eq, Show)

-- | Lambda form: (lambda (params...) body...)
data LambdaForm = LambdaForm
  { lambdaParams :: [Text]            -- ^ Parameter names
  , lambdaBody   :: [Expr]            -- ^ Body expressions
  }
  deriving (Eq, Show)

-- | Let form: (let ((name val)...) body...)
data LetForm = LetForm
  { letBindings :: [(Text, Expr)]     -- ^ (name, value) pairs
  , letBody     :: [Expr]              -- ^ Body expressions
  }
  deriving (Eq, Show)

-- | Define form: (define name value) or (define (name params...) body...)
data DefineForm
  = DefineVar Text Expr                -- ^ Variable definition
  | DefineFun Text [Text] [Expr]       -- ^ Function definition
  deriving (Eq, Show)

-- | If form: (if test then else)
data IfForm = IfForm
  { ifTest :: Expr
  , ifThen :: Expr
  , ifElse :: Expr                     -- ^ May be EmptyList for implicit #f
  }
  deriving (Eq, Show)

-- | Cond clause: (test body...) or (else body...)
data CondClause = CondClause
  { condTest :: Maybe Expr             -- ^ Nothing for 'else' clause
  , condBody :: [Expr]
  }
  deriving (Eq, Show)

-- | Get source location from an expression
exprLoc :: Expr -> SourceLoc
exprLoc expr = case expr of
  Const _ loc          -> loc
  Var _ loc             -> loc
  Lambda _ loc          -> loc
  Let _ loc              -> loc
  LetRec _ loc           -> loc
  Define _ loc           -> loc
  If _ loc               -> loc
  Cond _ loc             -> loc
  Application _ _ loc    -> loc
  Begin _ loc            -> loc
  CallCC _ loc           -> loc

-- | Check if an expression is a binding form
isBindingForm :: Expr -> Bool
isBindingForm expr = case expr of
  Lambda {}     -> True
  Let {}         -> True
  LetRec {}      -> True
  Define {}      -> True
  _              -> False

-- | Extract all variable references from an expression
-- This is a helper function for analysis, not used in main algorithm
extractVariables :: Expr -> [Text]
extractVariables = go []
  where
    go :: [Text] -> Expr -> [Text]
    go acc (Var name _)                    = name : acc
    go acc (Lambda form _)                 = foldr (\e acc' -> go acc' e) acc (lambdaBody form)
    go acc (Let form _)                     = 
      let bodyAcc = foldr (\e acc' -> go acc' e) acc (letBody form)
          bindingsAcc = foldr (\(_, e) acc' -> go acc' e) bodyAcc (letBindings form)
      in bindingsAcc
    go acc (LetRec form _)                  = 
      let bodyAcc = foldr (\e acc' -> go acc' e) acc (letBody form)
          bindingsAcc = foldr (\(_, e) acc' -> go acc' e) bodyAcc (letBindings form)
      in bindingsAcc
    go acc (Define (DefineVar _ e) _)       = go acc e
    go acc (Define (DefineFun _ _ body) _) = foldr (\e acc' -> go acc' e) acc body
    go acc (If form _)                     = go (go (go acc (ifTest form)) (ifThen form)) (ifElse form)
    go acc (Cond clauses _)                = 
      foldr (\clause acc' -> 
        let testAcc = maybe acc' (\test -> go acc' test) (condTest clause)
            bodyAcc = foldr (\e acc'' -> go acc'' e) testAcc (condBody clause)
        in bodyAcc) acc clauses
    go acc (Application fn args _)         = foldr (\e acc' -> go acc' e) (go acc fn) args
    go acc (Begin exprs _)                  = foldr (\e acc' -> go acc' e) acc exprs
    go acc (CallCC e _)                     = go acc e
    go acc _                                = acc

