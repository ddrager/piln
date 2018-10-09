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
\   select cid, ending_at, array_to_json(notes) from objects \
\ ) as o \
\ "
  encoder = Encoders.unit
  decoder = Decoders.singleRow (Decoders.column Decoders.json)

data Payment = Payment { _opennode_id :: S.Text
                       , _cid :: S.Text
                       , _amount :: Int64
                       , _time_bought :: DiffTime
                       , _note :: S.Text
                       }

savePayment :: Statement Payment ()
savePayment = Statement sql encoder decoder False
 where
  sql
    = "\
\ insert into payments \
\ (opennode_id, cid, amount, time_bought, actual_start, note) \
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
    (_opennode_id >$< Encoders.param Encoders.text)
      <> (_cid >$< Encoders.param Encoders.text)
      <> (_amount >$< Encoders.param Encoders.int8)
      <> (_time_bought >$< Encoders.param Encoders.interval)
      <> (_note >$< Encoders.param Encoders.text)
  decoder = Decoders.unit
