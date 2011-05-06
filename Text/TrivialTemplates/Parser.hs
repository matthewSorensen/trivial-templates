{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Text.TrivialTemplates.Parser(
  Chunk (..),
  Template,
  templateParser) where
import Data.Attoparsec hiding (satisfy, scan, takeWhile1)
import Data.Attoparsec.Char8  hiding (space)
import qualified Data.ByteString.Char8 as B
import Control.Applicative hiding (many)

data Chunk = Var B.ByteString |
             Lit B.ByteString |
             If B.ByteString [Chunk] [Chunk]
           deriving(Show)
                   
type Template = [Chunk]                   

templateParser::Parser Template
templateParser = template <* endOfInput
                
finishTag = () <$ (skipSpace >> string "}}")
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
                                          
tagType::B.ByteString->Parser ()
tagType tagType = skipSpace *> string tagType *> finishTag

template::Parser Template
template = filter validLit <$> temp 
  where temp = (:).Lit <$> literal <*> (continue <|> nothing)
        continue = skipSpace >> (:) <$> (iff <|> var) <*> temp
        var = Var <$> name <* finishTag
        iff = If  <$> tag  <*> template <*> (negative <|> nothing) <* tagType "end"
          where tag = string "if" *> space *> name <* finishTag
                negative = tagType "else" *> template
        name = takeWhile1 isAlpha_iso8859_15 >>= notKey
          where notKey x | (x=="end")||(x=="else") = empty
                         | otherwise               = pure x
        validLit (Lit "") = False
        validLit _        = True
        nothing = pure []