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
                                     , "absurd"
                                     , "inj"
                                     , "row"
                                     , "rec"
                                     , "do"
                                     , "handle"
                                     ]
           , Token.reservedOpNames = ["+", "*", "<-", "->", "\\", ":", "(", ")", "|"]
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
program :: Parser Comp
program = computation

computation :: Parser Comp
computation =  parens computation
           <|> letComp
           <|> appComp
           <|> splitComp
           <|> caseComp
           <|> absurdComp
           <|> retComp
           <|> doComp

letComp :: Parser Comp
letComp = do
    reserved "let"
    x <- identifier
    reservedOp "<-"
    c1 <- computation
    reserved "in"
    c2 <- computation
    return $ ELet x c1 c2


appComp :: Parser Comp
appComp = do
    v1 <- computation
    v2 <- computation
    return $ EApp v1 v2


splitComp :: Parser Comp
splitComp = do
    reserved "let"
    reservedOp "("
    label <- identifier
    reservedOp "="
    x <- identifier
    reservedOp ";"
    y <- identifier
    reservedOp ")"
    reservedOp "="
    v <- value
    reserved "in"
    comp <- computation
    return $ ESplit label x y v comp


caseComp :: Parser Comp
caseComp = do
    reserved "case"
    v <- value
    reservedOp "{"
    l <- identifier
    x <- identifier
    reservedOp "->"
    xC <- computation
    reservedOp ";"
    y <- identifier
    reservedOp "->"
    yC <- computation
    reservedOp "}"
    return $ ECase v l x xC y yC


absurdComp :: Parser Comp
absurdComp = do
    reserved "absurd"
    v <- value
    return $ EAbsurd v


retComp :: Parser Comp
retComp =  reserved "return"
        >> liftM EReturn value


doComp :: Parser Comp
doComp = do
    reserved "do"
    l <- identifier
    v <- value
    return $ EDo l v


handleComp :: Parser Comp
handleComp = do
    c <- computation
    h <- handler
    return $ EHandle c h


handler :: Parser Handler
handler = do
    reservedOp "{"
    h <- handlerBody
    reservedOp "}"
    return h


handlerBody :: Parser Handler
handlerBody =  parseHret
           <|> parseHops


parseHops :: Parser Handler
parseHops = do
    op <- parseAlgOp
    reservedOp "|"
    h <- handlerBody
    return $ HOps op h


parseAlgOp :: Parser AlgebraicOp
parseAlgOp = do
    l <- identifier
    p <- identifier
    r <- identifier
    reservedOp "->"
    c <- computation
    return $ AlgOp l p r c


parseHret :: Parser Handler
parseHret = do
    reserved "return"
    x <- identifier
    reservedOp "->"
    c <- computation
    return $ HRet x c


value :: Parser Value
value =  liftM VVar identifier
     <|> liftM VNum integer
     <|> lambdaExpr
     <|> fixOp
     <|> unit
     <|> extendRow
     <|> variantRow
     <|> expr

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

fixOp :: Parser Value
fixOp = do
    reserved "rec"
    g <- identifier
    x <- identifier
    reservedOp "->" -- todo: other syntax?
    c <- computation
    return $ VFix g x c

unit :: Parser Value
unit = reservedOp "(" >> reservedOp ")" >> return VUnit

extendRow :: Parser Value
extendRow = do
    reservedOp "("
    label <- identifier
    reservedOp "="
    v <- value
    reservedOp ";"
    vs <- value
    reservedOp ")"
    return $ VExtendRow label v vs

variantRow :: Parser Value
variantRow = do
    reserved "inj"
    label <- identifier
    v <- value
    reservedOp ":"
    t <- rowTypeAnnot
    return $ VVariantRow (VariantRow t label v)

rowTypeAnnot :: Parser RowType
rowTypeAnnot = reserved "row" >> return RowType -- todo

expr :: Parser Value
expr = buildExpressionParser binOps subExpr

binOps = [ [Infix  (reservedOp "*"   >> return (VBinOp BMul )) AssocLeft]
         , [Infix  (reservedOp "+"   >> return (VBinOp BAdd )) AssocLeft]
         ]

subExpr :: Parser Value
subExpr = parens expr <|> value

-- Local debugging tools
--
parseString :: String -> Comp
parseString str =
  case parse program "" str of
    Left e  -> error $ show e
    Right r -> r


parseFile :: String -> IO Comp
parseFile file =
  do code <- readFile file
     case parse program "" code of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
