{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module IPFS where

import           Data.Text
import           Data.Int

pinObject :: Text -> Either Text Int64
pinObject cid = Left "not implemented"
