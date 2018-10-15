{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Database where

import           Data.Text.Lazy                as T
import           Data.Text                     as S
import           Data.Int
import           Data.Time.Clock
import           Data.Functor.Contravariant
import           Data.Aeson                     ( Value )
import           Hasql.Statement                ( Statement(..) )
import qualified Hasql.Decoders                as Decoders
import qualified Hasql.Encoders                as Encoders

getObjects :: Statement () Value
getObjects = Statement sql encoder decoder False
 where
  sql
    = "\
\ select array_to_json(array_agg(row_to_json(o))) as objects \
\ from ( \
\   select cid, ending_at, notes from objects \
\ ) as o \
\ "
  encoder = Encoders.unit
  decoder = Decoders.singleRow (Decoders.column Decoders.json)

data Payment = Payment { order_id :: S.Text
                       , cid :: S.Text
                       , amount :: Int64
                       , time_bought :: DiffTime
                       , note :: S.Text
                       }

savePayment :: Statement Payment ()
savePayment = Statement sql encoder decoder False
 where
  sql
    = "\
\ insert into payments \
\ (order_id, cid, amount, time_bought, actual_start, note) \
\ values ($1, $2, $3, $4, ( \
\   select ending_at from ( \
\     select ending_at from objects where cid = $2 \
\       union all \
\     select now() as ending_at \
\   )x \
\   order by ending_at desc \
\   limit 1 \
\ ), $5) \
\ "
  encoder =
    (order_id >$< Encoders.param Encoders.text)
      <> (cid >$< Encoders.param Encoders.text)
      <> (amount >$< Encoders.param Encoders.int8)
      <> (time_bought >$< Encoders.param Encoders.interval)
      <> (note >$< Encoders.param Encoders.text)
  decoder = Decoders.unit

invoiceExists :: Statement S.Text Bool
invoiceExists = Statement sql encoder decoder False
 where
  sql
    = "\
\ select count(*) = 1 as count \
\ from payments \
\ where order_id = $1 \
\ "
  encoder = Encoders.param Encoders.text
  decoder = Decoders.singleRow (Decoders.column Decoders.bool)
