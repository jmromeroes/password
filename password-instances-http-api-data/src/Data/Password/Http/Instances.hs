{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Data.Password.Http.Instances
Copyright   : (c) Dennis Gosnell, 2019; Felix Paulusma, 2020
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This module provides additional typeclass instances
for 'Password' and 'PasswordHash'.

See the "Data.Password.Types" module for more information.
-}

module Data.Password.Http.Instances () where

import Data.Password.Types (Password, mkPassword, ErrMsg)
import GHC.TypeLits (TypeError)
import Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
--
-- Import needed functions.
--
-- >>> import Web.HttpApiData (parseUrlPiece)
-- >>> import Data.Password.Bcrypt (unsafeShowPassword)

-- | This instance allows a 'Password' to be created with functions like
-- 'Web.HttpApiData.parseUrlPiece' or 'Web.HttpApiData.parseQueryParam'.
--
-- >>> let eitherPassword = parseUrlPiece "foobar"
-- >>> fmap unsafeShowPassword eitherPassword
-- Right "foobar"
instance FromHttpApiData Password where
  parseUrlPiece = fmap mkPassword . parseUrlPiece

-- | Type error! Do not transmit plain-text 'Password's over HTTP!
instance TypeError (ErrMsg "HttpApiData") => ToHttpApiData Password where
  toUrlPiece = error "unreachable"
