{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Text                     as S
                                                ( findIndex
                                                , splitAt
                                                , tail
                                                )
import           Data.Text.Lazy                as T
                                                ( Text
                                                , pack
                                                , toStrict
                                                )
import           Data.Text.Lazy.Read
import           Data.Function                  ( (&) )
import           Data.Int
import qualified Data.ByteString.Char8         as C8
import           Data.Time.Clock
import           Web.Scotty                    as H
import           System.Environment
import           System.Exit
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.Cors
import           Network.HTTP.Types.Status
import           Data.Monoid                    ( (<>) )
import qualified Hasql.Connection              as Conn
import qualified Hasql.Session                 as Session

import           Lightning
import           Database

main :: IO ()
main = do
  port       <- fmap T.pack $ getEnv "PORT"
  connString <- fmap C8.pack $ getEnv "DATABASE_URL"
  mdb        <- Conn.acquire connString
  case mdb of
    Right db -> do
      scotty (parsePort port) $ do
        middleware logStdoutDev
        middleware simpleCors
        middleware $ staticPolicy (addBase "static")
        server db
    Left merr -> case merr of
      Just err -> die $ C8.unpack err
      Nothing  -> die "failed to open postgres connection"

server :: Conn.Connection -> ScottyM ()
server db = do
  H.get "/" $ do
    setHeader "Content-Type" "text/html"
    file "static/index.html"

  H.post "/api/pay" $ do
    cid  <- H.param "cid"
    note <- H.param "note"
    amt  <- fmap decimal $ H.param "amount"
    case amt of
      Left err -> do
        status status400
        html $ "<h1>error</h1><p>wrong amount: " <> (T.pack err) <> "</p>"
      Right (amount, _) -> do
        inv <- liftIO $ makeInvoice (cid, note, amount)
        liftIO $ print inv
        html inv

  H.post "/callback/payment" $ do
    invoice_id <- fmap toStrict $ H.param "id"

    amt        <- H.param "price"
    let amount = case decimal amt of
          Left  _               -> 0
          Right (a :: Int64, _) -> a

    desc <- fmap toStrict $ H.param "description"
    let (cid, note) = case S.findIndex ((==) ':') desc of
          Just idx ->
            let (cid, note) = S.splitAt idx desc in (cid, S.tail note)
          Nothing -> (desc, "")

    if amount < 77
      then do
        status status400
      else do
        paid <- liftIO $ isInvoicePaid invoice_id

        if paid
          then do
            let paid_days = fromIntegral amount / 77 & round
            let time      = secondsToDiffTime $ paid_days * 86400
            let payment   = Payment invoice_id cid amount time note
            res <- liftIO
              $ Session.run (Session.statement (payment) savePayment) db

            case res of
              Left err -> do
                liftIO $ print err
                status status500
              Right _ -> do
                liftIO $ print "saved payment on db"
                status status200
          else status status403

  H.get "/api/objects" $ do
    res <- liftIO $ Session.run (Session.statement () getObjects) db
    case res of
      Left err -> do
        status status500
        html $ "<h1>error</h1>" <> (err & show & T.pack)
      Right oo -> json oo

parsePort :: Text -> Int
parsePort port = case decimal port of
  Left  _      -> 3000
  Right (n, _) -> n
