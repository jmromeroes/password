{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module      : Data.Password.Aeson.Instances
Copyright   : (c) Dennis Gosnell, 2019; Felix Paulusma, 2020
License     : BSD-style (see LICENSE file)
Maintainer  : cdep.illabout@gmail.com
Stability   : experimental
Portability : POSIX

This module provides Aeson typeclass instances for 'Password'

See the "Data.Password.Types" module for more information.
-}

module Data.Password.Aeson.Instances () where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Password.Types (Password, mkPassword, ErrMsg)
import GHC.TypeLits (TypeError)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
--
-- Import needed functions.
--
-- >>> import Data.Aeson (decode)
-- >>> import Data.Password.Bcrypt (Salt(..), hashPasswordWithSalt, unsafeShowPassword)

-- | This instance allows a 'Password' to be created from a JSON blob.
--
-- >>> let maybePassword = decode "\"foobar\"" :: Maybe Password
-- >>> fmap unsafeShowPassword maybePassword
-- Just "foobar"
--
-- There is no instance for 'ToJSON' for 'Password' because we don't want to
-- accidentally encode a plain-text 'Password' to JSON and send it to the end-user.
--
-- Similarly, there is no 'ToJSON' and 'FromJSON' instance for 'PasswordHash'
-- because we don't want to accidentally send the password hash to the end
-- user.
instance FromJSON Password where
  parseJSON = fmap mkPassword . parseJSON

instance TypeError (ErrMsg "JSON") => ToJSON Password where
  toJSON = error "unreachable"
