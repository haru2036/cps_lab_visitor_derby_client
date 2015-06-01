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
  cardId <- readPasori
  print cardId
  --throwRequest $ C.pack $ encode $ toJSON $ PostJSON $ C.unpack cardId

fcfId = snd . splitAt 2 . fst . splitAt 9

fcfRetake = snd . splitAt 14

readPasori :: IO (ByteString, ByteString)
readPasori = do
  maybePasori <- pasoriPrepare
  case maybePasori of
    Just pasori -> do
      maybeFelicaPtr <- felicaPolling 0xfe00 0 0 pasori 
      let felicaPtr = fromJust maybeFelicaPtr
      felica <- withForeignPtr felicaPtr peek 
      print "IDm is:"
      print felica
      print "PMm is:"
      print $ pmm felica
      maybeBlockWord <- withForeignPtr felicaPtr $ felicaReadSingle 0 0x1A8B 0
      maybeBlockWord2 <- withForeignPtr felicaPtr $ felicaReadSingle 0 0x1A8B 1
      maybeBlockWord3 <- withForeignPtr felicaPtr $ felicaReadSingle 0 0x1A8B 2
      maybeBlockWord4 <- withForeignPtr felicaPtr $ felicaReadSingle 0 0x1A8B 3
      let cardId = fcfRetake $ convert "SHIFT-JIS" "UTF-8" $ pack $ fromJust maybeBlockWord 
      let cardName = convert "SHIFT-JIS" "UTF-8" $ pack $ fromJust maybeBlockWord2
      let block3 = convert "SHIFT-JIS" "UTF-8" $ pack $ fromJust maybeBlockWord3
      let block4 = convert "SHIFT-JIS" "UTF-8" $ pack $ fromJust maybeBlockWord
      print block3
      print block4
      pasoriClose pasori
      return (cardId, cardName)
    Nothing -> error "Pasori is not connected"


