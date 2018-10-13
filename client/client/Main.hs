{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Prelude
import           GHC.Generics
import           Data.Aeson
import           Miso
import           Miso.String                    ( MisoString
                                                , pack
                                                )
import           JavaScript.Web.XMLHttpRequest

data Model
  = Model
    { objects :: [PObject]
    , selected :: Maybe MisoString
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

instance FromJSON PObject where
  parseJSON = withObject "pobject" $ \o ->
    PObject <$> o .: "cid" <*> o .: "notes" <*> .: "ending_at"

main :: IO ()
main = do
  startApp App {..}
 where
  initialAction = FetchObjects
  model   = Model
    { objects  = []
    , selected = Nothing
    , invoice  = Nothing
    , err      = Nothing
    , loading  = False
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
  | Pay MisoString
  deriving (Show, Eq)

updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp         m = noEff m

updateModel FetchObjects m = m <# do
  SetPObjects <$> getPObjects

updateModel (SetPObjects pobjs) m = noEff m { objects = pobjs }

updateModel (Pay id) m@Model {..} = noEff m

viewModel :: Model -> View Action
viewModel Model {..} = div_ [] [ul_ [] $ map viewPObject objects]

viewPObject :: PObject -> View Action
viewPObject PObject {..} = li_ [] [text cid]

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
