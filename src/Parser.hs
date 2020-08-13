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
           , Token.identStart      = letter <|> char '_'
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "in"
                                     , "let"
                                     , "return"
                                     , "true"
                                     , "false"
                                     , "int"
                                     , "bool"
                                     , "absurd"
                                     , "row"
                                     , "rec"
                                     , "case"
                                     , "if"
                                     , "then"
                                     , "else"
                                     , "do"
                                     , "handle"
                                     , "with"
                                     , "observe"
                                     , "cohandle"
                                     , "through"
                                     ]
           , Token.reservedOpNames = [
                "+", "*", "<-", "->", "\\", ":", "(", ")", "|",
                "<", ">", ">=", "<=", "==", "!=", ","
            ]
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
computation =  letBasedComp
           <|> caseComp
           <|> ifComp
           <|> absurdComp
           <|> retComp
           <|> opComp
           <|> coopComp
           <|> handleComp
           <|> cohandleComp
           <|> appComp


letBasedComp :: Parser Comp
letBasedComp = do
    reserved "let"
    letComp <|> splitComp

letComp :: Parser Comp
letComp = do
    x <- identifier
    reservedOp "<-"
    c1 <- computation
    reserved "in"
    ELet x c1 <$> computation

appComp :: Parser Comp
appComp = EApp <$> value <*> value


splitComp :: Parser Comp
splitComp = do
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
    ESplit label x y v <$> computation


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


ifComp :: Parser Comp
ifComp = do
    reserved "if"
    cond <- value
    reserved "then"
    c <- computation
    reserved "else"
    EIf cond c <$> computation


absurdComp :: Parser Comp
absurdComp = do
    reserved "absurd"
    EAbsurd <$> value


retComp :: Parser Comp
retComp = reserved "return"
        >> EReturn <$> value


opComp :: Parser Comp
opComp = do
    reserved "do"
    EOp <$> identifier <*> value


coopComp :: Parser Comp
coopComp = do
    reserved "observe"
    ECoop <$> identifier <*> value


handleComp :: Parser Comp
handleComp = do
    reserved "handle"
    c <- computation
    reserved "with"
    EHandle c <$> handler

cohandleComp :: Parser Comp
cohandleComp = do
    reserved "cohandle"
    c <- computation
    reserved "through"
    ECohandle c <$> handler


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
    HOps op <$> handlerBody


parseAlgOp :: Parser AlgebraicOp
parseAlgOp = do
    l <- identifier
    p <- identifier
    r <- identifier
    reservedOp "->"
    AlgOp l p r <$> computation


parseHret :: Parser Handler
parseHret = do
    reserved "return"
    x <- identifier
    reservedOp "->"
    HRet x <$> computation

value :: Parser Value
value =  lambdaExpr
     <|> fixOp
     <|> parensValue
     <|> extendRow
     <|> variantRow
     <|> expr

parensValue = do
    reservedOp "("
    unit <|> pair

pair :: Parser Value
pair = do
    v1 <- value
    reservedOp ","
    v2 <- value
    reservedOp ")"
    return $ VPair v1 v2

lambdaExpr :: Parser Value
lambdaExpr = do
    reservedOp "\\"
    x <- identifier
    reservedOp ":"
    t <- typeAnnot
    reservedOp "->"
    VLambda x t <$> computation

typeAnnot :: Parser ValueType
typeAnnot =  (reserved "int" >> return TInt)
         <|> (reserved "bool" >> return TBool)

fixOp :: Parser Value
fixOp = do
    reserved "rec"
    g <- identifier
    x <- identifier
    reservedOp "->" -- todo: other syntax?
    VFix g x <$> computation

unit :: Parser Value
unit = reservedOp ")" >> return VUnit

extendRow :: Parser Value
extendRow = do
    reservedOp "{"
    label <- identifier
    reservedOp "="
    v <- value
    reservedOp ";"
    vs <- value
    reservedOp "}"
    return $ VExtendRow label v vs

variantRow :: Parser Value
variantRow = do
    reservedOp "["
    label <- identifier
    v <- value
    reservedOp "]"
    reservedOp ":"
    t <- rowTypeAnnot
    return $ VVariantRow (VariantRow t label v)

rowTypeAnnot :: Parser RowType
rowTypeAnnot = reserved "row" >> return RowType -- todo

expr :: Parser Value
expr = buildExpressionParser binOps term

binOps = [ [
            Infix  (reservedOp "<="   >> return (VBinOp BLte )) AssocLeft,
            Infix  (reservedOp "<"   >> return (VBinOp BLt )) AssocLeft,
            Infix  (reservedOp ">="   >> return (VBinOp BGte )) AssocLeft,
            Infix  (reservedOp ">"   >> return (VBinOp BGt )) AssocLeft,
            Infix  (reservedOp "=="   >> return (VBinOp BEq )) AssocLeft,
            Infix  (reservedOp "!="   >> return (VBinOp BNe )) AssocLeft
           ]
         , [
            Infix  (reservedOp "*"   >> return (VBinOp BMul )) AssocLeft,
            Infix  (reservedOp "/"   >> return (VBinOp BDiv )) AssocLeft
           ]
         , [
            Infix  (reservedOp "+"   >> return (VBinOp BAdd )) AssocLeft,
            Infix  (reservedOp "-"   >> return (VBinOp BSub )) AssocLeft
           ]
         ]

term :: Parser Value
term =  parens value
    <|> fmap VVar identifier
    <|> fmap VNum integer
    <|> stringTerm

escape :: Parser String
escape = do
    d <- char '\\'
    escapable <- oneOf "\\\"0nrvtbf"
    return [d, escapable]

nonEscape :: Parser String
nonEscape = many (noneOf "\\\"\0\n\r\v\t\b\f")

character :: Parser String
character = nonEscape <|> escape

stringTerm :: Parser Value
stringTerm = do
    char '"'
    str <- character
    char '"'
    whiteSpace
    return $ VStr str

-- Parser runners
--

parseString :: String -> Either Error Comp
parseString s = case parse program "" s of
    Left e -> Left $ ParseError $ show e
    Right r -> return r


parseString' :: String -> Comp
parseString' str =
  case parseString str of
    Left e  -> error $ show e
    Right r -> r
