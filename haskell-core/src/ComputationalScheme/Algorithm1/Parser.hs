{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | S-expression parser for R5RS Scheme
--
-- Uses megaparsec to parse Scheme source code into AST.
-- Supports a core subset of R5RS without macros initially.
module ComputationalScheme.Algorithm1.Parser where

import ComputationalScheme.Algorithm1.AST
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void, when)
import Control.Applicative hiding (many, some, Const)
import Data.Void (Void)
import Data.Maybe (isNothing)

type Parser = Parsec Void Text

-- | Parse a complete Scheme program (list of expressions)
parseProgram :: Text -> Either (ParseErrorBundle Text Void) [Expr]
parseProgram = MP.parse programParser ""

-- | Main parser for a program (list of top-level expressions)
programParser :: Parser [Expr]
programParser = spaceConsumer *> many exprParser <* eof

-- | Parse a single expression
exprParser :: Parser Expr
exprParser = lexeme $ choice
  [ constantParser
  , variableParser
  , listParser
  ]

-- | Parse constants (numbers, strings, booleans, characters, quoted)
constantParser :: Parser Expr
constantParser = lexeme $ do
  loc <- getSourcePos
  let srcLoc = SourceLoc (unPos (sourceLine loc)) (unPos (sourceColumn loc)) 0
  choice
    [ Const <$> numberConstant <*> pure srcLoc
    , Const <$> stringConstant <*> pure srcLoc
    , Const <$> boolConstant <*> pure srcLoc
    , Const <$> charConstant <*> pure srcLoc
    , Const <$> quoteConstant <*> pure srcLoc
    ]

numberConstant :: Parser Constant
numberConstant = Number <$> L.signed space L.float

stringConstant :: Parser Constant
stringConstant = String . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

boolConstant :: Parser Constant
boolConstant = choice
  [ Bool True <$ string "#t"
  , Bool False <$ string "#f"
  ]

charConstant :: Parser Constant
charConstant = char '#' <* char '\\' *> (Char <$> anySingle)

quoteConstant :: Parser Constant
quoteConstant = char '\'' *> (Quote <$> exprParser)

-- | Parse variable (identifier)
variableParser :: Parser Expr
variableParser = lexeme $ do
  loc <- getSourcePos
  let srcLoc = SourceLoc (unPos (sourceLine loc)) (unPos (sourceColumn loc)) 0
  name <- identifier
  return $ Var name srcLoc

-- | Parse identifier (variable name)
identifier :: Parser Text
identifier = T.pack <$> some (alphaNumChar <|> oneOf "-!$%&*+/:<=>?@^_~")

-- | Parse list forms (lambda, let, define, if, cond, application, etc.)
listParser :: Parser Expr
listParser = lexeme $ between (char '(') (char ')') $ do
  loc <- getSourcePos
  let srcLoc = SourceLoc (unPos (sourceLine loc)) (unPos (sourceColumn loc)) 0
  
  -- Peek at first symbol to determine form type
  first <- lookAhead (identifier <* space)
  
  case T.unpack first of
    "lambda"  -> lambdaParser srcLoc
    "let"     -> letParser srcLoc
    "letrec"  -> letRecParser srcLoc
    "define"  -> defineParser srcLoc
    "if"      -> ifParser srcLoc
    "cond"    -> condParser srcLoc
    "begin"   -> beginParser srcLoc
    "call-with-current-continuation" -> callCCParser srcLoc
    "call/cc" -> callCCParser srcLoc
    _         -> applicationParser srcLoc

-- | Parse lambda: (lambda (params...) body...)
lambdaParser :: SourceLoc -> Parser Expr
lambdaParser srcLoc = do
  void (string "lambda" <* space)
  params <- parens (many identifier)
  body <- some exprParser
  return $ Lambda (LambdaForm params body) srcLoc

-- | Parse let: (let ((name val)...) body...)
letParser :: SourceLoc -> Parser Expr
letParser srcLoc = do
  void (string "let" <* space)
  bindings <- parens $ many $ parens $ do
    name <- identifier
    space
    val <- exprParser
    return (name, val)
  body <- some exprParser
  return $ Let (LetForm bindings body) srcLoc

-- | Parse letrec: (letrec ((name val)...) body...)
letRecParser :: SourceLoc -> Parser Expr
letRecParser srcLoc = do
  void (string "letrec" <* space)
  bindings <- parens $ many $ parens $ do
    name <- identifier
    space
    val <- exprParser
    return (name, val)
  body <- some exprParser
  return $ LetRec (LetForm bindings body) srcLoc

-- | Parse define: (define name val) or (define (name params...) body...)
defineParser :: SourceLoc -> Parser Expr
defineParser srcLoc = do
  void (string "define" <* space)
  name <- identifier
  space
  
  -- Check if next is list (function definition) or expression (variable definition)
  choice
    [ do
        params <- parens $ many identifier
        body <- some exprParser
        return $ Define (DefineFun name params body) srcLoc
    , do
        val <- exprParser
        return $ Define (DefineVar name val) srcLoc
    ]

-- | Parse if: (if test then else)
ifParser :: SourceLoc -> Parser Expr
ifParser srcLoc = do
  void (string "if" <* space)
  test <- exprParser
  space
  thenExpr <- exprParser
  space
  elseExpr <- exprParser
  return $ If (IfForm test thenExpr elseExpr) srcLoc

-- | Parse cond: (cond (test body...)... (else body...))
condParser :: SourceLoc -> Parser Expr
condParser srcLoc = do
  void (string "cond" <* space)
  clauses <- many $ parens $ do
    test <- optional (try exprParser)
    when (isNothing test) $ void (string "else" <* space)
    body <- some exprParser
    return $ CondClause test body
  return $ Cond clauses srcLoc

-- | Parse begin: (begin expr...)
beginParser :: SourceLoc -> Parser Expr
beginParser srcLoc = do
  void (string "begin" <* space)
  exprs <- some exprParser
  return $ Begin exprs srcLoc

-- | Parse call/cc: (call/cc proc) or (call-with-current-continuation proc)
callCCParser :: SourceLoc -> Parser Expr
callCCParser srcLoc = do
  void (choice [string "call/cc", string "call-with-current-continuation"] <* space)
  proc <- exprParser
  return $ CallCC proc srcLoc

-- | Parse application: (func arg...)
applicationParser :: SourceLoc -> Parser Expr
applicationParser srcLoc = do
  func <- exprParser
  args <- many exprParser
  return $ Application func args srcLoc

-- | Parse parenthesized expression
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

-- | Lexeme parser (consumes trailing whitespace)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Space consumer (handles comments and whitespace)
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

