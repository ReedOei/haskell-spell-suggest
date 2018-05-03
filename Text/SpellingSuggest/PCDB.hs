{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Copyright © 2010 Greg Weber and Bart Massey
-- [This program is licensed under the "3-clause ('new') BSD License"]
-- Please see the file COPYING in this distribution for license information.

-- | Create and maintain a nonvolatile database of
--   phonetic codes.
module Text.SpellingSuggest.PCDB (
   defaultDB,
   createDB, openDB, matchDB
 ) where

import qualified Control.Exception as C
import Data.Maybe
import qualified Data.Text as T
import Database.SQLite.Simple
import Text.PhoneticCode.Soundex
import Text.PhoneticCode.Phonix

-- import Paths_spelling_suggest;

-- | File path for default cache database.
defaultDB :: IO String
defaultDB = return "spelling-suggest.sq3"

-- | Create and populate the phonetic codes database, given
-- a list of words and a database path.
createDB :: [String] -> Maybe String -> IO Connection
createDB ws dbPath = do
  defaultDBPath <- defaultDB
  db <- open $ fromMaybe defaultDBPath dbPath

  execute_ db "DROP TABLE IF EXISTS phonetic_codes;"
  execute_ db "CREATE TABLE IF NOT EXISTS phonetic_codes (word text, soundex text, phonix text);"

  execute_ db "BEGIN TRANSACTION;"

  mapM_ (insert db) ws

  execute_ db "COMMIT;"
  return db

  where
    insert db = executeNamed db insertSt . makeParams

    insertSt = "INSERT INTO phonetic_codes (word, soundex, phonix) VALUES (:word, :soundex, :phonix);"

    makeParams w = [":word" := w, ":soundex" := soundex True w, ":phonix" := phonix w]

-- | Open the phonetic codes database, given a database path.
openDB :: Maybe String -> IO (Maybe Connection)
openDB dbPath = do
  defaultDBPath <- defaultDB
  conn :: Either C.SomeException Connection <- C.try $ open $ fromMaybe defaultDBPath dbPath

  case conn of
    Left _ -> return Nothing
    Right db -> return $ Just db

  where
    openDBFile dbp =
      C.catch (do db <- open dbp
                  return (Just db))
        (const (return Nothing) :: C.IOException -> IO (Maybe Connection))

-- | Return all the words in the given coding system matching the given code.
matchDB :: Connection -> String -> String -> IO [String]
matchDB db coding code = map fromOnly <$> queryNamed db
      (Query (T.pack ("SELECT word FROM phonetic_codes WHERE " ++ coding ++ " = :code ;")))
      [":code" := code]

