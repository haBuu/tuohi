module Handler.ImportPlayers where

import Import hiding ((.:))

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Text.Encoding as E
import qualified Data.Vector as V
import Data.Char (ord)
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as C

import Handler.Forms
import Handler.Division
import Model.User

getImportPlayersR :: CompetitionId -> Handler Html
getImportPlayersR cid = do
  competition <- runDB $ get404 cid
  ((_, formWidget), formEnctype) <- importPlayersForm
  defaultLayout $ do
    setTitleI MsgImportPlayers
    $(widgetFile "import-players")

postImportPlayersR :: CompetitionId -> Handler Html
postImportPlayersR cid = do
  ((result, _), _) <- importPlayersForm
  formHandler result $ \csvData -> do
    -- convert from Textarea to lazy ByteString
    -- Textarea -> Text -> strict ByteString -> lazy ByteString
    let lazyBS = BL.fromChunks [E.encodeUtf8 $ unTextarea csvData]
    case decodeByNameWith options lazyBS of
      Left err -> setMessageI $ MsgPlayersImportError $ pack err
      Right (_, players) -> do
        runDB $ do
          -- delete all current sign-ups
          deleteWhere [SignUpCompetitionId ==. cid]
          V.forM_ (players :: V.Vector PDGAPlayer) $ \p -> do
            let name = (firstName p) ++ " " ++ (lastName p)
            uid <- insertUserNoEmail $ pack name
            void $ insertUnique $ SignUp uid cid True (division p)
          setMessageI MsgPlayersImported
  redirect $ CompetitionR cid

data PDGAPlayer = PDGAPlayer
  { division :: !Division
  , firstName :: !String
  , lastName :: !String
  , pdga :: !(Maybe Int)
  } deriving (Show)

instance FromNamedRecord PDGAPlayer where
  parseNamedRecord r = PDGAPlayer
    <$> r .: "Division"
    <*> r .: "First name"
    <*> r .: "Last name"
    <*> r .: "PDGA"

instance FromField Division where
  parseField s = case readMaybe $ C.unpack s of
    Just d -> pure d
    Nothing -> mzero

-- decoding options for Data.Csv
options :: DecodeOptions
options = defaultDecodeOptions
  -- use tabulator as delimiter
  { decDelimiter = fromIntegral $ ord ('\t')
  }