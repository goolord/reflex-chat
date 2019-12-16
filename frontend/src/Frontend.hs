{-# LANGUAGE 
    DataKinds
  , OverloadedStrings
  , TypeApplications 
  , RecursiveDo
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC
    -fno-warn-unused-imports
    -fno-warn-name-shadowing
#-}

module Frontend where

import Common.Api
import Common.Route
import Control.Monad
import Control.Applicative
import Data.List (intersperse)
import Data.Foldable (traverse_, foldl', sequenceA_)
import Data.Functor
import Data.Default.Class
import Data.FileEmbed
import Data.Map (Map)
import Data.Text (Text)
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex
import Reflex.Dom
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Data.Witherable
import qualified GHCJS.DOM.EventM as GHCJS
import Control.Applicative.Backwards
import Data.Attoparsec.Text

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      layoutMain chat
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

chat :: forall t m.
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  ) => m ()
chat = mdo
  br
  chatBuffer <- holdDyn [] $
    attachWith 
      newline
      (current chatBuffer)
      commandE
  divClass "card chat" $ dyn_ $ fmap renderChat chatBuffer
  br
  let renderChat :: [Command] -> m ()
      renderChat [] = br
      renderChat xs = traverse_ (command user) xs

  (user, commandE) <- divClass "chat-input" $ do
    user <- inputD ("placeholder" =: "User" <> "style" =: "width: 20%;")
    ti <- inputW ("placeholder" =: "Send a message")
    let commandE = ffor ti $ \input -> either (Send . T.pack) id $ parseOnly parseCommand input
    pure (user, commandE)
  blank
  where
  newline :: [Command] -> Command -> [Command]
  newline _ Clear = []
  newline as x = x:as

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

data Command
  = Clear
  | Me Text
  | Send Text

br :: DomBuilder t m => m ()
br = el "br" blank

parseCommand :: Parser Command
parseCommand = do
  mcomm <- optional $ char '/'
  case mcomm of
    Nothing -> Send <$> takeText
    Just _ -> do
          (string "me " *> fmap Me takeText)
      <|> (string "clear" $> Clear)
      <|> ( do
              rest <- takeText
              pure $ Send $ "/" <> rest
          )

command :: (PostBuild t m, DomBuilder t m) => Dynamic t Text -> Command -> m ()
command _ Clear = blank
command user (Me x) = el "div" $ do
  dynText user *> text " "
  text x
command user (Send x) = el "div" $ do 
  el "strong" (dynText user) *> text ": "
  text x
