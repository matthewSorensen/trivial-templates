module Text.TrivialTemplates(
  Enviroment,
  Template,  
  safeParseTemplate,
  readTemplate,
  safeReadTemplate,
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

parseTemplate::ByteString->Template
parseTemplate = unsafeEnd . parse

safeParseTemplate::ByteString->Either String Template
safeParseTemplate = Atto.eitherResult . parse

readTemplate::FilePath->IO Template              
readTemplate = fmap parseTemplate . readFile

safeReadTemplate::FilePath->IO (Either String Template)
safeReadTemplate = fmap safeParseTemplate . readFile

parse::ByteString->Atto.Result Template
parse = flip Atto.feed empty . Atto.parse templateParser

unsafeEnd::Atto.Result a->a
unsafeEnd = either error id . Atto.eitherResult

writeAsBuilder::Template->Enviroment->Builder
writeAsBuilder tmp env = mconcat $ map (write' env) tmp
  where write' _ (Lit a) = fromByteString a
        write' e (If a true false) =maybe f  t $ lookup a e
          where f = writeAsBuilder false e
                t _ = writeAsBuilder true e
        write' e (Var n) = fromByteString $ e!n

writeAsByteString::Template->Enviroment->ByteString
writeAsByteString = (toByteString .) . writeAsBuilder
