{-# LANGUAGE DeriveGeneric #-}

module WorkingStack.Data where

import System.IO.Error

import Data.Aeson
import GHC.Generics
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as ByteString

data Entry = Entry {
  description :: Text.Text,
  notes :: Text.Text } deriving (Generic, Show)

data WorkingStack = WorkingStack [Entry] deriving (Generic, Show)

instance ToJSON Entry where
  -- No need to provide a toJSON implementation.
  
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON Entry
  -- No need to provide a parseJSON implementation.

instance ToJSON WorkingStack where
  -- No need to provide a toJSON implementation.
  
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions
  
instance FromJSON WorkingStack
  -- No need to provide a parseJSON implementation.
  
--------------------------------------------------------------------------------

loadWorkingStackFromFile file = do
  result <- tryIOError (ByteString.readFile file)
  case result of
    Left ex -> return ([])
    Right bs -> do
      case decode bs :: Maybe WorkingStack of
        Just (WorkingStack xs) -> return xs
        Nothing -> return ([])

--------------------------------------------------------------------------------

saveWorkingStackToFile ws file = do
  ByteString.writeFile file (encode ws)

--------------------------------------------------------------------------------
