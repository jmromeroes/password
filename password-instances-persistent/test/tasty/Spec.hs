{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Text (Text)
import Database.Persist.Class (PersistField(..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Web.HttpApiData (FromHttpApiData(..))

import Data.Password.Types (Password, PasswordHash(..), unsafeShowPassword)
import Data.Password.Persistent.Instances ()

main :: IO ()
main = defaultMain $ testGroup "Password Instances" [ persistTest ]

persistTest :: TestTree
persistTest = testProperty "PasswordHash (PersistField)" $ \pass ->
    let pwd = PasswordHash pass
    in fromPersistValue (toPersistValue pwd) === Right pwd
