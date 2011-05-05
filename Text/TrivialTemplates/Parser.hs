{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Text.TrivialTemplates.Parser(
  Chunk (..),
  Template,
  templateParser) where

import Data.Attoparsec hiding (satisfy, scan, takeWhile1)
import Data.Attoparsec.Char8  hiding (space)
import qualified Data.ByteString.Char8 as B
import Control.Applicative hiding (many)

type ByteString = B.ByteString

data Chunk = Var ByteString |
             Lit ByteString |
             If ByteString [Chunk] [Chunk]
           deriving(Show)
                   
type Template = [Chunk]                   

templateParser::Parser Template
templateParser = template <* endOfInput
                
end = () <$ string "}}"
space = satisfy isSpace >> skipSpace

literal = B.concat . reverse <$> literal' []
  where literal' acc = scan (-1,False) update >>= post acc
        post acc bs  = case B.splitAt (B.length bs -2) bs of 
          (a,"{{")  -> return   $ a:acc
          (a,"\\{") -> literal' $ "{":a:acc
          otherwise -> return   $ bs:acc
        update (state,term) char 
          | term                        = Nothing --Terminate!
          | state < 0  && char == '{'   = Just (0::Int, term)
          | 0 <= state && char == '{'   = Just (state,True)
          | 0 <= state && char == '\\'  = Just (state + 1,term)
          | otherwise                   = Just (-1, term)
var = True
             
varOrIf::Parser (Bool,ByteString)
varOrIf = skipSpace >> (i <|> v) <* skipSpace <* end
  where i = string "if" >> space >> (not var,) <$> name
        v = (var,) <$> (name>>= failIfKey)
        name = takeWhile1 isAlpha_iso8859_15
        failIfKey s
          | s == "end" || s == "else" = fail ""
          | otherwise = return s
tagType::ByteString->Parser ()
tagType tagType = skipSpace >> string tagType >> skipSpace >> end

template::Parser Template
template = filter validLit <$> temp 
  where temp = (:).Lit <$> literal <*> (continue <|> return [])
        continue = (:) <$> (try varOrIf >>= rest) <*> temp
        rest (tagType,name)
          | tagType == var  = return $ Var name
          | otherwise       = cond name
        validLit (Lit "") = False
        validLit _        = True
cond::ByteString->Parser Chunk
cond pro = If pro <$> template <*> (neg <|> none) <* tagType "end"
  where neg = tagType "else" >> template
        none = return []

