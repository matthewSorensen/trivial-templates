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
  where literal' acc = scan (0::Int,0::Int) update >>= post acc
        update (n,m) char
          | (m == 0) && char =='{'   = Just (n+1,0)
          | (2 <= n) && char == '\\' = Just (n,m+1)
          | (m > 0)  && char /= '\\' = Nothing
          | (2 <= n) && char /= '{'  = Nothing
          | otherwise                = Just (0,0)
        post acc bs = case B.splitAt (B.length bs -2) bs of
          (a,"{{") -> return $ a:acc
          (a, "{\\") -> literal' $ "{":a:acc
          (a, "\\\\") -> literal' $ "\\":a:acc
          otherwise   -> return $ bs:acc

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