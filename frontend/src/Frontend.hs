{-# LANGUAGE 
    DataKinds
  , OverloadedStrings
  , TypeApplications 
  , RecursiveDo
  , ScopedTypeVariables
  , DeriveGeneric
  , FlexibleContexts
  , JavaScriptFFI
  , CPP
#-}

{-# OPTIONS_GHC
    -fno-warn-unused-imports
    -fno-warn-name-shadowing
#-}

module Frontend where

import Common.Api
import Common.Route
import Control.Applicative
import Control.Applicative.Backwards
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Attoparsec.Text
import Data.Default.Class
import Data.FileEmbed
import Data.Foldable (traverse_, foldl', sequenceA_)
import Data.Functor
import Data.List (intersperse)
import Data.Map (Map)
import Data.Text (Text)
import Data.Witherable
import GHC.Generics (Generic(..))
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex
import Reflex.Dom hiding (Command)
import Language.Javascript.JSaddle.Evaluate
import qualified Chronos as C
import qualified Data.Dequeue as DQ
import qualified Data.Text as T
import qualified GHCJS.DOM.EventM as GHCJS
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Types    as T


js_offset :: DOM.JSM T.JSVal
js_offset = eval ("new Date().getTimezoneOffset()" :: String)

getOffset :: DOM.JSM C.Offset
getOffset = do
  offset <- DOM.fromJSValUnchecked =<< js_offset
  pure $ case offset of
    0 -> C.Offset 0
    _ -> C.Offset $ offset `div` 60

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      offset <- prerender (pure $ C.Offset 0) $ DOM.liftJSM getOffset
      layoutMain $ chat offset
  }

layoutMain :: (DomBuilder t m) => m a -> m a
layoutMain child = do
  css $ static @"chota.css"
  css $ static @"main.css"
  elClass "main" "container" $ do
    child

css :: DomBuilder t m => Text -> m ()
css x = 
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: x) blank

chat :: forall t m js.
  ( PostBuild t m
  , DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , Prerender js t m
  ) => Dynamic t C.Offset 
  -> m ()
chat offset = mdo
  br
  chatBuffer <- holdDyn [] $
    attachWith 
      newline
      (current chatBuffer)
      commandE
  divClass "card chat" $ dyn_ $ fmap renderChat chatBuffer
  br
  let renderChat :: [Command] -> m ()
      renderChat [] = blank
      renderChat xs = traverse_ command xs

  commandE <- divClass "chat-input" $ do
    user <- inputD ("placeholder" =: "User" <> "style" =: "width: 20%;")
    ti <- inputW ("placeholder" =: "Send a message")
    fmap (switch . current) . prerender (pure never) $ performEvent $ ffor ti $ \input -> do
      let commandType = either (Send . T.pack) id $ parseOnly parseCommand input
      now <- liftIO C.now
      user' <- sample $ current user
      pure $ Command user' now commandType
  blank
  where
  newline :: [Command] -> Command -> [Command]
  newline _ (Command _ _ Clear) = []
  newline as x = x:as
  command :: Command -> m ()
  command (Command  _ _ Clear) = blank
  command (Command user _ (Me x)) = el "div" $ do
    text user *> text " "
    text x
  command (Command user time (Html _x)) = el "div" $ do
    renderUser user time offset
  command (Command user time (Send x)) = el "div" $ do 
    renderUser user time offset
    text x

inputD :: (DomBuilder t m) => Map AttributeName Text -> m (Dynamic t T.Text)
inputD attrs = do
  input <- inputElement $ def
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs
    -- inputElement with content reset on send
  pure $ value input

inputW :: (DomBuilder t m, MonadFix m) => Map AttributeName Text -> m (Event t T.Text)
inputW attrs = mdo
  let send = keypress Enter input <> domEvent Click send2
      -- send signal firing on *return* key press
  input <- inputElement $ def
    & inputElementConfig_setValue .~ (send $> "")
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ attrs
  (send2, _) <- elAttr' "button" ("class" =: "chat-button") $ text "Send"
  -- inputElement with content reset on send
  pure $ tag (current $ _inputElement_value input) send

data CommandType
  = Clear
  | Me Text
  | Send Text
  | Html Text
  deriving (Generic)

instance ToJSON CommandType
instance FromJSON CommandType

data Command = Command Text C.Time CommandType
  deriving (Generic)

instance ToJSON Command
instance FromJSON Command

br :: DomBuilder t m => m ()
br = el "br" blank

parseCommand :: Parser CommandType
parseCommand = do
  mcomm <- optional $ char '/'
  case mcomm of
    Nothing -> Send <$> takeText
    Just _ -> do
          (string "me " *> fmap Me takeText)
      <|> (string "clear" $> Clear)
      <|> (string "html " *> fmap Html takeText)
      <|> ( do
              rest <- takeText
              pure $ Send $ "/" <> rest
          )

renderUser :: forall t m. (DomBuilder t m, PostBuild t m) => Text -> C.Time -> Dynamic t C.Offset -> m ()
renderUser user now offsetD = do
  el "strong" (text user) *> text ": " *> elClass "span" "weak" (dynText $ encodeTime now)
  br
  where
  encodeTime :: C.Time -> Dynamic t Text
  encodeTime (C.Time x) = ffor offsetD $ \(C.Offset hours) -> 
    C.encode_YmdHMS (C.SubsecondPrecisionFixed 0) C.slash $ C.timeToDatetime $ C.Time $ x - (fromIntegral $ hours * 3600000000000)
  
tshow :: Show a => a -> Text
tshow = T.pack . show

encodeMinutes :: Int -> Text
encodeMinutes m
  | m < 10 = "0" <> tshow m
  | otherwise = tshow m
