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
  Enviroment,
  Template,  
  readTemplate,
  parseTemplate,
  writeAsBuilder,
  writeAsByteString
  ) where
import Prelude hiding (readFile,lookup)
import qualified Data.Attoparsec as Atto
import Blaze.ByteString.Builder
import Data.ByteString (readFile,empty,ByteString)
import Data.Monoid (mconcat)
import Data.Map (Map,lookup,(!)) 
import Text.TrivialTemplates.Parser

type Enviroment = Map ByteString ByteString

-- | Parses a template from a 'Data.ByteString', using the template syntax previously outlined. 
--  If the template fails to parse, a 'Data.Either.Left' value containing an error message.
parseTemplate::ByteString->Either String Template
parseTemplate = Atto.eitherResult . parse

-- | Reads and parses a template from a file using 'Data.ByteString.readFile'
readTemplate::FilePath->IO (Either String Template)
readTemplate = fmap parseTemplate . readFile

parse::ByteString->Atto.Result Template
parse = flip Atto.feed empty . Atto.parse templateParser

-- | Renders a template to a 'Blaze.ByteString.Builder.Builder'. If any variables referenced 
--   in the template aren't present in the 'Environment' an error will be thrown.
writeAsBuilder::Template->Enviroment->Builder
writeAsBuilder tmp env = mconcat $ map (write' env) tmp
  where write' _ (Lit a) = fromByteString a
        write' e (If pro true false) =maybe f  t $ lookup pro e
          where f = writeAsBuilder false e
                t _ = writeAsBuilder true e
        write' e (Var name) = maybe err fromByteString $ lookup name e
          where err = error $ "Key " ++ show name ++ " not present in environment"
          
-- | Identical to 'writeAsBuilder', except it renders a template to a 'Data.ByteString'
writeAsByteString::Template->Enviroment->ByteString
writeAsByteString = (toByteString .) . writeAsBuilder
