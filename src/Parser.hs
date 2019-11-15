{-| BNF Grammar

program : computation
        | expr
        | empty

computation : LET var <- computation in computation
            | RETURN expr
            | value value

expr : ( expr )
     | binary_expr
     | value

binary_expr : expr op expr

op : + | *

value : number
      | bool
      | var
      | lambda

lambda : \ var -> computation

var : id

empty :

-}
module Parser where


import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import AST
import Types

-- Lexer
--
languageDef =
  emptyDef { Token.commentStart    = "(--"
           , Token.commentEnd      = "--)"
           , Token.commentLine     = "--"
           , Token.nestedComments  = True
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "in"
                                     , "let"
                                     , "return"
                                     , "true"
                                     , "false"
                                     , "int"
                                     , "bool"
                                     ]
           , Token.reservedOpNames = ["+", "*", "<-", "->", "\\", ":"]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
whiteSpace = Token.whiteSpace lexer -- parses whitespace

-- Parser
--
program :: Parser Term
program = term

term :: Parser Term
term =  liftM TermExpr expr
    <|> liftM TermComp computation

computation :: Parser Comp
computation =  parens computation
           <|> letComp
           <|> retComp
           <|> appComp

letComp :: Parser Comp
letComp = do
    reserved "let"
    x <- identifier
    reservedOp "<-"
    c1 <- computation
    reserved "in"
    c2 <- computation
    return $ CLet x c1 c2

retComp :: Parser Comp
retComp =  reserved "return"
        >> liftM CRet expr


appComp :: Parser Comp
appComp = do
    v1 <- value
    v2 <- value
    return $ CApp v1 v2


expr :: Parser Expr
expr = buildExpressionParser binOps subExpr

binOps = [ [Infix  (reservedOp "*"   >> return (EBinOp BMul )) AssocLeft]
         , [Infix  (reservedOp "+"   >> return (EBinOp BAdd )) AssocLeft]
         ]

subExpr :: Parser Expr
subExpr = parens expr <|> valueExpr

valueExpr :: Parser Expr
valueExpr = liftM EVal value

value :: Parser Value
value =  liftM VVar identifier
     <|> liftM VNum integer
     <|> lambdaExpr
     <|> boolTrue
     <|> boolFalse

lambdaExpr :: Parser Value
lambdaExpr = do
    reservedOp "\\"
    x <- identifier
    reservedOp ":"
    t <- typeAnnot
    reservedOp "->"
    c <- computation
    return $ VLambda x t c

typeAnnot :: Parser ValueType
typeAnnot =  (reserved "int" >> return TInt)
         <|> (reserved "bool" >> return TBool)

boolTrue :: Parser Value
boolTrue = reserved "true" >> return (VBool True)

boolFalse :: Parser Value
boolFalse = reserved "false" >> return (VBool False)


-- Local debugging tools
--
parseString :: String -> Term
parseString str =
  case parse program "" str of
    Left e  -> error $ show e
    Right r -> r


parseFile :: String -> IO Term
parseFile file =
  do code <- readFile file
     case parse program "" code of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
