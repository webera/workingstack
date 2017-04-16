{-# LANGUAGE DeriveGeneric #-}

module WorkingStack.Data where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as DataText
import qualified Data.ByteString.Lazy as ByteString

data Entry = Entry {
  description :: DataText.Text,
  notes :: DataText.Text } deriving (Generic, Show)

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
  bs <- ByteString.readFile file
  case decode bs :: Maybe WorkingStack of
    Just ws -> return ws
    Nothing -> return (WorkingStack [])

--------------------------------------------------------------------------------

saveWorkingStackToFile ws file = do
  ByteString.writeFile file (encode ws)

--------------------------------------------------------------------------------

{-
class IsGValue Entry where

  toGValue :: a -> IO GValue
  toGValue entry = do
    return ()

  fromGValue :: GValue -> IO a
  fromGValue = do
    
    return ()
-}

