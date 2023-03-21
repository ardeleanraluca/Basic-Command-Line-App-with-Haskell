module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import Data.List qualified as L
import Entry.DB qualified as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import Prelude qualified

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  DB.save DB.empty
  return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  db <- DB.load
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
      -- let id = map (\x -> entryId x) (DB.fromSnippetDB db')
      let ent = DB.findFirst (\x -> entryId x == getOptId getOpts) db'
      case ent of
        Nothing -> putStrLn "error"
        Just entt -> putStrLn (entrySnippet entt)

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  db <- DB.load
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
      let mat = DB.findAll (matchedByAllQueries (searchOptTerms searchOpts)) db'
      case mat of
        [] -> putStrLn "No entries found"
        _ -> putStrLn (foldl (++) "" (map (\x -> show (FmtEntry x) ++ "\n") mat))

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  snip <- readFile (addOptFilename addOpts)
  db <- DB.load
  case db of
    (Error r) -> putStrLn "Failed to load DB"
    (Success db') -> do
      let ent = DB.findFirst (\x -> entrySnippet x == snip) db'
      case ent of
        Just entt -> putStrLn ("Entry with this content already exists: \n" ++ show (FmtEntry entt))
        Nothing -> do
          DB.modify
            ( DB.insertWith
                ( \id ->
                    Entry
                      { entryId = id,
                        entrySnippet = snip,
                        entryFilename = addOptFilename addOpts,
                        entryLanguage = addOptLanguage addOpts,
                        entryDescription = addOptDescription addOpts,
                        entryTags = addOptTags addOpts
                      }
                )
            )
          return ()

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
