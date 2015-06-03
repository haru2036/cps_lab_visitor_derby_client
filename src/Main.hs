{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
import Bindings.Libpafe.Pasori
import Bindings.Libpafe.Types
import Bindings.Libpafe.Felica
import Prelude hiding (splitAt)
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe
import Data.Typeable
import Codec.Text.IConv
import Text.JSON
import Text.JSON.Generic 
import Network.HTTP.Client
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

-- 今はnameだけど将来的にcardIdにします
data PostJSON = PostJSON {
                name :: String
                }deriving (Eq, Show, Typeable, Data)


hogeRequest = PostJSON "test"

throwRequest :: ByteString -> IO ()
throwRequest body = do
  initReq <- parseUrl "http://cps.miraitoarumachi.com/people.json"
  let req = initReq
              { method = "POST"
               ,requestBody = RequestBodyLBS body
               ,requestHeaders = [("Content-Type", "application/json")
                                 ,("Accept", "application/json")]
              }
  withManager defaultManagerSettings (\x -> httpNoBody req x >> return())

main = do
  pasoriToThrow
  
pasoriToThrow = do
  cardId <- runMaybeT readPasori
  print $ fromJust cardId
  --throwRequest $ C.pack $ encode $ toJSON $ PostJSON $ C.unpack cardId

fcfId = snd . splitAt 2 . fst . splitAt 9

fcfRetake = snd . splitAt 14


readPasori :: MaybeT IO (ByteString)
readPasori = do
      pasori <- MaybeT pasoriPrepare
      felicaPtr <- MaybeT $ felicaPolling 0xfe00 0 0 pasori 
      felica <- liftIO $ withForeignPtr felicaPtr peek 
      blockWord <- MaybeT $ withForeignPtr felicaPtr $ felicaReadSingle 0 0x1A8B 0
      let cardId = fcfId $ convert "SHIFT-JIS" "UTF-8" $ pack blockWord
      liftIO $ print cardId
      liftIO $ pasoriClose pasori
      return cardId
