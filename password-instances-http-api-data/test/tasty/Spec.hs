{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.QuickCheck.Instances.Text ()
import Web.HttpApiData (FromHttpApiData(..))

import Data.Password.Types (unsafeShowPassword)
import Data.Password.Http.Instances()

main :: IO ()
main = defaultMain $ testGroup "Password Instances" [ fromHttpApiDataTest ]

fromHttpApiDataTest :: TestTree
fromHttpApiDataTest = testCase "Password (FromHttpApiData)" $
    assertEqual "password doesn't match" (Right testPassword) $
      unsafeShowPassword <$> parseUrlPiece testPassword
  where
    testPassword = "passtest"
