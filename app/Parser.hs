{-# LANGUAGE RecordWildCards #-}
module Parser where

import Text.Parsec (eof, sepBy1, (<|>), parse, try, ParseError, SourceName)
import Text.Parsec.Expr (buildExpressionParser, Operator(Prefix, Infix), Assoc(AssocLeft))
import Text.Parsec.Language (javaStyle)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as ParsecToken
import Text.Parsec.Prim (ParsecT)

import Data.Functor.Identity (Identity)

import Interpreter (AExp (..), BExp (..), Stmt (..), Id (..))

-- creates a TokenParser record and destructures its fields (eg. identifier,
--  reservedOp, etc.) so that we can use them directly
ParsecToken.TokenParser {..} = ParsecToken.makeTokenParser javaStyle

binOp :: String -> (a -> a -> a) -> Operator String u Identity a
binOp name fun = Infix (fun <$ reservedOp name) AssocLeft

varId :: ParsecT String u Identity Id
varId = Id <$> identifier

aexp :: Parser AExp
aexp = buildExpressionParser table term
  where term =  Lit . fromIntegral <$> integer
            <|> Var <$> varId
            <|> parens aexp
        table = [ [ binOp "+" (:+:), binOp "-" (:-:) ],
                  [ binOp "*" (:*:), binOp "/" (:/:) ] ]

bexp :: Parser BExp
bexp = buildExpressionParser table term
  where term =  True' <$ reserved "true"
            <|> False' <$ reserved "false"
            <|> try ((:<=:) <$> (aexp <* reservedOp "<=") <*> aexp)
            <|> try ((:==:) <$> (aexp <* reservedOp "==") <*> aexp)
            <|> try (parens bexp)
        table = [ [ Prefix (Not <$ reservedOp "!") ],
                  [ binOp "&&" (:&:), binOp "||" (:|:) ] ]

stmt :: Parser Stmt
-- the entire structure will be a sequence (Seq) of statements
stmt = foldl Seq Skip <$> sepBy1 statement (symbol ";")
  where statement =  If <$> (reserved "if" *> bexp)
                        <*> braces stmt
                        <*> (reserved "else" *> braces stmt)
                 <|> While <$> (reserved "while" *> bexp)
                           <*> braces stmt
                 <|> Set <$> (varId <* reservedOp ":=") <*> aexp

parseSrc :: SourceName -> String -> Either ParseError Stmt
parseSrc = parse (stmt <* eof)
