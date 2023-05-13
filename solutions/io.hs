{-# Language DeriveGeneric #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}

import Control.Monad
import Data.Aeson
import Data.Maybe
import GHC.Generics
import Network.HTTP.Simple
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC8

names :: [String]
names = [ "Alex", "Erez", "Irfan", "Matthias", "Petr", "Tyler" ]

mkRequest :: String -> IO ()
mkRequest name = do
  response <- httpBS (parseRequest_ ("https://api.nationalize.io/?name=" ++ name))
  BC8.putStrLn (getResponseBody response)

main :: IO ()
main = mapM_ mkRequest names

-- data Person = Person { person_name :: String, person_nationalities :: [Nationality] } deriving Show

-- data Nationality = Nationality { country_id :: String, probability :: Float } deriving (Show, Generic)

-- instance FromJSON Nationality where

-- instance FromJSON Person where
--   parseJSON = withObject "" $ \v -> Person <$> v .: ("name" :: T.Text) <*> v .: ("country" :: T.Text)

-- main :: IO ()
-- main = forM_ names $ \name -> do
--   response <- httpLBS (parseRequest_ ("https://api.nationalize.io/?name=" ++ name))
--   let body = getResponseBody response
--   let person = fromJust (decode @Person body)
--       pName = person_name person
--       pNat  = country_id . head . person_nationalities $ person
--   print (pName, pNat)