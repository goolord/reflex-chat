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

import Prelude hiding (filter)
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
import Data.Functor.Identity
import Data.Functor.Sum
import Data.List (intersperse)
import GHCJS.DOM.Element (setInnerHTML)
import Data.List.NonEmpty (nonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.These
import Data.Witherable
import GHC.Generics (Generic(..))
import Language.Javascript.JSaddle.Evaluate
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Obelisk.Route.TH
import Reflex
import Reflex.Dom hiding (Command)
import Text.URI
import Control.Lens ((^.))
import Language.Javascript.JSaddle
  (jsg, js, js1, jss, fun, valToNumber, syncPoint, (<#))
import qualified Chronos as C
import qualified Data.Dequeue as DQ
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import qualified GHCJS.DOM.EventM as GHCJS
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.Foreign  as F
import qualified GHCJS.Types as T
import qualified Obelisk.ExecutableConfig.Lookup as Cfg

js_offset :: DOM.JSM T.JSVal
js_offset = eval ("new Date().getTimezoneOffset()" :: String)

getOffset :: DOM.JSM C.Offset
getOffset = do
  offset <- DOM.fromJSValUnchecked =<< js_offset
  pure $ case offset of
    0 -> C.Offset 0
    _ -> C.Offset $ offset `div` 60

unsafeRawHtml :: (Prerender js t m, DomBuilder t m) => Text -> m ()
unsafeRawHtml html = do
  prerender_ blank $ do
    (element, _) <- el' "div" blank
    DOM.liftJSM $ setInnerHTML (_element_raw element) html

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      offset <- prerender (pure $ C.Offset 0) $ DOM.liftJSM getOffset
      route <- (fmap . fmap) T.decodeUtf8 $ getConfig "common/route"
      layoutMain $ chat offset route
  }

layoutMain :: (DomBuilder t m) => m a -> m a
layoutMain child = do
  css "https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css"
  css $ static @"main.css"
  elClass "main" "container" $ do
    child

css :: DomBuilder t m => Text -> m ()
css x = 
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: x) blank

staticJs :: DomBuilder t m => Text -> m ()
staticJs x =
  elAttr "script" ("src" =: x) blank

chat :: forall t m js.
  ( PostBuild t m
  , DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , Prerender js t m
  ) 
  => Dynamic t C.Offset 
  -> Maybe Text
  -> m ()
chat offset mroute = mdo
  staticJs $ static @"chat.js"
  br
  commandSocket :: Dynamic t (RawWebSocket t (Maybe Command)) <- 
    case checkEncoder backendRouteEncoder of
      Left err -> do
        el "div" $ text err
        fail ("checkEncoder error: " <> T.unpack err)
      Right encoder -> do
        let wsPath = fst $ encode encoder $ InL BackendRoute_WebSocketChat :/ ()
        let mUri = do
            uri' <- mkURI =<< mroute
            pathPiece <- nonEmpty =<< mapM mkPathPiece wsPath
            wsScheme <- case uriScheme uri' of
              rtextScheme | rtextScheme == mkScheme "https" -> mkScheme "wss"
              rtextScheme | rtextScheme == mkScheme "http" -> mkScheme "ws"
              _ -> Nothing
            pure $ uri'
              { uriPath = Just (False, pathPiece)
              , uriScheme = Just wsScheme
              }
        case mUri of
          Nothing -> fail $ "no uri: " <> show mroute
          Just uri ->
            prerender (pure $ RawWebSocket never never never never) $ jsonWebSocket (render uri) 
              (WebSocketConfig
                { _webSocketConfig_send = fmap pure commandE
                , _webSocketConfig_close = never 
                , _webSocketConfig_reconnect = True
                , _webSocketConfig_protocols = []
                }
              )
  let socketCommands = catMaybes $ switch $ current $ fmap _webSocket_recv commandSocket
  chatBuffer <- holdDyn [] $
    attachWith 
      newline
      (current chatBuffer)
      socketCommands
  divClass "chat card" $ elClass "div" "chat-internal" $ do
    void $ dynamicList
      (\_index c _cE -> command c)
      (\_ -> filter (\x -> commandType x == Clear) socketCommands $> () )
      (const never)
      socketCommands
      []
  br
  commandE <- divClass "chat-input" $ do
    user <- inputD ("placeholder" =: "User" <> "style" =: "width: 20%;" <> "class" =: "input")
    ti <- inputW ("placeholder" =: "Send a message" <> "id" =: "message-input" <> "class" =: "input")
    fmap (switch . current) . prerender (pure never) $ performEvent $ ffor ti $ \input -> do
      let commandType = either (Send . T.pack) id $ parseOnly parseCommand input
      now <- liftIO C.now
      user' <- sample $ current user
      pure $ Command user' now commandType
  blank
  where
  newline :: [Command] -> Command -> [Command]
  newline _ (Command _ _ Clear) = []
  newline as a = a : as
  command :: Command -> m ()
  command (Command  _ _ Clear) = blank
  command (Command user _ (Me x)) = el "div" $ do
    text user *> text " "
    el "div" $ text x
  command (Command user time (Html x)) = el "div" $ do
    renderUser user time offset
    el "div" $ unsafeRawHtml x
  command (Command user time (JS x)) = el "div" $ do
    renderUser user time offset
    el "pre" $ el "code" $ text x
    el "div" $ prerender_ blank $ void $ DOM.liftJSM $ eval x
  command (Command user time (Send x)) = el "div" $ do 
    renderUser user time offset
    el "div" $ text x

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
  (send2, _) <- elAttr' "button" ("class" =: "chat-button button is-primary") $ text "Send"
  -- inputElement with content reset on send
  pure $ tag (current $ _inputElement_value input) send

data CommandType
  = Clear
  | Me Text
  | Send Text
  | Html Text
  | JS Text
  deriving (Generic, Eq, Show)

instance ToJSON CommandType
instance FromJSON CommandType

data Command = Command 
  { commandUser :: Text
  , commandTime :: C.Time 
  , commandType :: CommandType
  }
  deriving (Generic, Show)

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
      <|> (string "js " *> fmap JS takeText)
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

------------------------------------------------------------------------------
-- | Dynamic list widget that creates a list that supports the dynamic
-- addition and removal of items.  This widget is completely general with zero
-- markup-specific choices.  It handles all the event plumbing and lets you
-- completely determine the markup.
dynamicList :: forall a t b m.
      ( MonadFix m
      , Adjustable t m
      , MonadHold t m
      )
    => (Int -> a -> Event t a -> m b)
    -- ^ Widget used to display each item
    -> (b -> Event t ())
    -- ^ Function that gets a remove event from the return value of each item
    -> (b -> Event t a)
    -- ^ Event that adds a new item to the list that is somehow based on an
    -- existing item.  If you don't want anything like this, use `const never`.
    -> Event t a
    -- ^ Event that adds a new item to the list that is not based on an
    -- existing item.
    -> [a]
    -- ^ Initial list of items
    -> m (Dynamic t [b])
dynamicList w removeEvent addFunc addEvent initList = do
    let initMap = M.fromList $ zip [0..] initList
    rec 
      let vals :: Event t (Map Int (Maybe a))
          vals = mergeWith (<>)
            [ attachWith addNew (current res) addEvent
            , addSpecific (current res)
            , remove (current res)
            ]
      res <- listWithKeyShallowDiff initMap vals w
    pure $ M.elems <$> res
  where
    addSpecific res = switch (foo <$> res)
    foo m = leftmost $ map (fmap (addNew m) . addFunc) $ M.elems m
    addNew m a = M.singleton k (Just a)
      where
        k = if M.null m then 0 else fst (M.findMax m) + 1
    remove res = switch (mergeWith (<>) . map f . M.toList <$> res)
      where
        f (k,b) = M.singleton k Nothing <$ removeEvent b
