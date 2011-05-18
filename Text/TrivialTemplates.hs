{-| A simple (even /trivial/) template system, offering simple substitution 
    and conditional inclusion.

A short demonstration of substitution, literals, and conditional inclusion:

>import Data.ByteString.Char8 (pack)
>import Text.TrivialTemplates
>import Data.Map (fromList)
> 
>env = fromList . map (\(key,val)->(pack key, pack val)) 
>template = toTemplate "hello, {{object}} {{\\ {{if this}}that{{else}}not that{{end}}"
>  where toTemplate = either error id . parseTemplate . pack
>main = do
>  print $ writeAsByteString template $ env [("object","world"),("this","")]
>  print $ writeAsByteString template $ env [("object","world")]

-}
module Text.TrivialTemplates(
  Env,
  Template,  
  Stacked (..),
  readTemplate,
  parseTemplate,
  writeAlt,
  writeApp,  
  writeFunc
  ) where
import Prelude hiding (readFile,lookup)
import qualified Data.Attoparsec as Atto
import Blaze.ByteString.Builder
import Data.ByteString (readFile,ByteString)
import qualified Data.ByteString as B
import Data.Monoid (mconcat,mappend,mempty)
import Data.Map (Map,lookup,(!)) 
import Data.Maybe
import Text.TrivialTemplates.Parser
import Control.Applicative 

type Env a = (ByteString->a ByteString)

writeAlt::Alternative a=>Env a->Template->a Builder
writeAlt env = foldl (\acc chunk-> mappend <$> acc <*> get env chunk) $ pure mempty
  where get _ (Lit a)             = pure $ fromByteString a
        get e (If pro true false) = (e pro *> writeAlt e true) <|> writeAlt e false
        get e (Var name)          = fromByteString <$> e name

writeApp::Applicative a=>Env a->Template->a Builder
writeApp env = fmap fromJust . runStack . writeAlt (Stacked . fmap pure . env)

writeFunc::(ByteString->ByteString)->Template->Builder
writeFunc e = fromJust . writeAlt (pure . e)


newtype Stacked m n a = Stacked {runStack::m (n a)}
instance (Functor m, Functor n) => Functor (Stacked m n) where
  fmap f (Stacked x) = Stacked $ (f <$>) <$> x
instance (Applicative m, Applicative n) => Applicative (Stacked m n) where
  pure = Stacked . pure . pure
  (Stacked a) <*> (Stacked b) = Stacked $ (<*>) <$> a <*> b
instance (Applicative m, Alternative n) => Alternative (Stacked m n) where
  empty = Stacked (pure empty)
  (Stacked a) <|> (Stacked b) = Stacked $ (<|>) <$> a <*>  b

-- | Parses a template from a 'Data.ByteString', using the template syntax previously outlined. 
--  If the template fails to parse, a 'Data.Either.Left' value containing an error message.
parseTemplate::ByteString->Either String Template
parseTemplate = Atto.eitherResult . parse

-- | Reads and parses a template from a file using 'Data.ByteString.readFile'
readTemplate::FilePath->IO (Either String Template)
readTemplate = fmap parseTemplate . readFile

parse::ByteString->Atto.Result Template
parse = flip Atto.feed B.empty . Atto.parse templateParser
