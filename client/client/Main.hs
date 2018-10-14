{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Prelude
import           GHC.Generics
import           Data.Scientific
import           Data.HashMap.Strict            ( fromList )
import           Data.Aeson
import           Miso
import           Miso.String                    ( MisoString
                                                , toMisoString
                                                , fromMisoString
                                                , pack
                                                )
import           Data.Monoid
import           JavaScript.Web.XMLHttpRequest

data Model
  = Model
    { objects :: [PObject]
    , adding :: Maybe AObject
    , invoice :: Maybe MisoString
    , err :: Maybe MisoString
    , loading :: Bool
    } deriving (Eq, Show, Generic)

data PObject
  = PObject
    { cid :: MisoString
    , notes :: [MisoString]
    , ending_at :: MisoString
    } deriving (Eq, Show, Generic)

data AObject
  = AObject
    { acid :: MisoString
    , anote :: MisoString
    , amount :: Int
    } deriving (Eq, Show, Generic)

instance FromJSON PObject where
  parseJSON = withObject "pobject" $ \o ->
    PObject <$> o .: "cid" <*> o .: "notes" <*> o .: "ending_at"

main :: IO ()
main = do
  startApp App {..}
 where
  initialAction = FetchObjects
  model         = Model
    { objects = []
    , adding  = Nothing
    , invoice = Nothing
    , err     = Nothing
    , loading = False
    }
  update     = updateModel
  view       = viewModel
  events     = defaultEvents
  mountPoint = Just "app"
  subs       = []

data Action
  = NoOp
  | FetchObjects
  | SetPObjects [PObject]
  | StartAddingNew
  | ChangeNewCid MisoString
  | ChangeNewNote MisoString
  | ChangeNewAmount MisoString
  | StartExtendingOld MisoString
  | RequestInvoice
  | GotInvoice MisoString
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp         m = noEff m

updateModel FetchObjects m = m <# do
  SetPObjects <$> getPObjects

updateModel (SetPObjects pobjs) m = noEff m { objects = pobjs }

updateModel StartAddingNew m =
  noEff m { adding = Just $ AObject {acid = "", anote = "", amount = 100} }
updateModel (StartExtendingOld cid) m@Model {..} =
  noEff m { adding = Just $ AObject {acid = cid, anote = "", amount = 100} }

updateModel (ChangeNewCid value) m@Model {..} = case adding of
  Nothing   -> noEff m
  Just aobj -> noEff m { adding = Just $ aobj { acid = value } }
updateModel (ChangeNewNote value) m@Model {..} = case adding of
  Nothing   -> noEff m
  Just aobj -> noEff m { adding = Just $ aobj { anote = value } }
updateModel (ChangeNewAmount value) m@Model {..} =
  let intvalue :: Int
      intvalue = fromMisoString value
  in  case adding of
        Nothing   -> noEff m
        Just aobj -> noEff m { adding = Just $ aobj { amount = intvalue } }

updateModel RequestInvoice m@Model {..} = case adding of
  Nothing   -> noEff m
  Just aobj -> m { loading = True } <# do
    GotInvoice <$> getInvoice aobj

updateModel (GotInvoice value) m = noEff m { invoice = Just value }

viewModel :: Model -> View Action
viewModel Model {..} = div_
  []
  [ div_ [class_ "pin-new"]
         [button_ [onClick StartAddingNew] [text "pin new IPFS object"]]
  , case adding of
    Nothing   -> text ""
    Just aobj -> viewAObject aobj
  , ul_ [class_ "pobjects"] $ map viewPObject objects
  , case invoice of
    Nothing  -> text ""
    Just inv -> viewInvoice inv
  ]

viewInvoice :: MisoString -> View Action
viewInvoice inv = div_ [class_ "invoice"] [text inv]

viewAObject :: AObject -> View Action
viewAObject AObject {..} = div_
  [class_ "aobject"]
  [ form_
      [onSubmit RequestInvoice]
      [ label_ [] [input_ [onInput ChangeNewCid, value_ acid]]
      , label_ [] [input_ [onInput ChangeNewNote, value_ anote]]
      , label_
        []
        [ input_
            [ onInput ChangeNewAmount
            , type_ "number"
            , min_ "100"
            , step_ "1"
            , value_ $ toMisoString amount
            ]
        ]
      , button_ [] [text "pay"]
      ]
  ]

viewPObject :: PObject -> View Action
viewPObject PObject {..} = li_
  []
  [ div_
      []
      [ p_
        []
        [ a_
            [ href_ $ "https://cloudflare-ipfs.com/ipfs/" <> cid
            , target_ "_blank"
            ]
            [text cid]
        ]
      , p_ [] [text $ "ending at " <> ending_at]
      , div_ [] $ map viewNote notes
      , div_
        []
        [button_ [onClick (StartExtendingOld cid)] [text "increase pin time"]]
      ]
  ]

viewNote :: MisoString -> View Action
viewNote note = div_ [class_ "note"] [text note]

getPObjects :: IO [PObject]
getPObjects = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String [PObject] of
    Left  s -> error s
    Right j -> pure j
 where
  req = Request
    { reqMethod          = GET
    , reqURI             = pack "/api/objects"
    , reqLogin           = Nothing
    , reqHeaders         = []
    , reqWithCredentials = False
    , reqData            = NoData
    }

getInvoice :: AObject -> IO MisoString
getInvoice aobj@AObject { acid, anote, amount } = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String String of
    Left  s -> error s
    Right j -> pure $ pack j
 where

  req = Request
    { reqMethod          = POST
    , reqURI             = pack "/api/pay"
    , reqLogin           = Nothing
    , reqHeaders         = [("Content-Type", "application/json")]
    , reqWithCredentials = False
    , reqData = StringData $ toMisoString $ encode $ Object $ fromList
      [ ("cid", String $ fromMisoString acid)
      , ("note", String $ fromMisoString anote)
      , ("amount", Number $ scientific (toInteger amount) 0)
      ]
    }
