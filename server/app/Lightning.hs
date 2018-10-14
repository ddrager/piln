{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Lightning where

import           Data.Monoid                    ( (<>) )
import           Data.Function                  ( (&) )
import           System.Environment
import           Data.Text                     as S
                                                ( Text
                                                , unpack
                                                )
import           Data.Text.Lazy                as T
                                                ( Text )
import qualified Data.ByteString.Char8         as C8
import           Data.UUID.V4                   ( nextRandom )
import           Data.UUID                      ( toText )
import           Network.Wreq                  as W
                                                ( getWith
                                                , postWith
                                                , defaults
                                                , header
                                                , responseBody
                                                )
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , lazy
                                                )
import           Data.Aeson                     ( (.=)
                                                , object
                                                )
import           Data.Aeson.Lens                ( key
                                                , _String
                                                )

isInvoicePaid :: S.Text -> IO Bool
isInvoicePaid id = do
  opennode_url <- getEnv "OPENNODE_URL"
  opennode_key <- getEnv "OPENNODE_KEY"
  let opts =
        W.defaults
          &  W.header "Content-Type"
          .~ ["application/json"]
          &  W.header "Authorization"
          .~ [C8.pack opennode_key]
  r <- W.getWith opts (opennode_url ++ "/charge/" ++ (S.unpack id))
  let status = r ^. responseBody . key "data" . key "status" . _String
  pure $ status == "paid"

makeInvoice :: (T.Text, T.Text, Int) -> IO T.Text
makeInvoice (cid, note, amount) = do
  opennode_url <- getEnv "OPENNODE_URL"
  opennode_key <- getEnv "OPENNODE_KEY"
  service_url  <- getEnv "SERVICE_URL"
  let callback_url = service_url ++ "/callback/payment"
  uuid <- fmap toText nextRandom
  let opts =
        W.defaults
          &  W.header "Content-Type"
          .~ ["application/json"]
          &  W.header "Authorization"
          .~ [C8.pack opennode_key]
  let body = object
        [ "description" .= (cid <> ":" <> note)
        , "amount" .= amount
        , "order_id" .= uuid
        , "callback_url" .= callback_url
        ]
  r <- W.postWith opts (opennode_url <> "/charges") body

  pure
    $  r
    ^. responseBody
    .  key "data"
    .  key "lightning_invoice"
    .  key "payreq"
    .  _String
    .  lazy
