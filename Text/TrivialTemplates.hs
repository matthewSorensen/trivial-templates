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

parseTemplate::ByteString->Either String Template
parseTemplate = Atto.eitherResult . parse

readTemplate::FilePath->IO (Either String Template)
readTemplate = fmap parseTemplate . readFile

parse::ByteString->Atto.Result Template
parse = flip Atto.feed empty . Atto.parse templateParser

writeAsBuilder::Template->Enviroment->Builder
writeAsBuilder tmp env = mconcat $ map (write' env) tmp
  where write' _ (Lit a) = fromByteString a
        write' e (If pro true false) =maybe f  t $ lookup pro e
          where f = writeAsBuilder false e
                t _ = writeAsBuilder true e
        write' e (Var name) = maybe err fromByteString $ lookup name e
          where err = error $ "Key " ++ show name ++ " not present in enviroment"
          
writeAsByteString::Template->Enviroment->ByteString
writeAsByteString = (toByteString .) . writeAsBuilder
