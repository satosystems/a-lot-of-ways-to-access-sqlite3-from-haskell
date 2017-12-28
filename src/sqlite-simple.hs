{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Safe
import Database.SQLite.Simple ( FromRow(..)
                              , Only(..)
                              , SQLError
                              , ToRow(..)
                              , close
                              , execute
                              , execute_
                              , open
                              , query
                              )
import Database.SQLite.Simple.FromRow (field)

data File = File String (Maybe Int) (Maybe String) deriving Show

instance FromRow File where
  fromRow = File <$> field <*> field <*> field

instance ToRow File where
  toRow (File path size digest) = toRow (path, size, digest)

main :: IO ()
main = do
  conn <- open "sqlite-simple.sqlite3"
  (execute_ conn "create table file (path text primary key, size integer, digest text)") `catch` (const $ return () :: SQLError -> IO ())
  execute conn "insert into file values (?, ?, ?)" (File "/foo.txt" (Just 100) Nothing)
  execute conn "insert into file values (?, ?, ?)" (File "/bar.txt" Nothing Nothing)
  execute conn "insert into file values (?, ?, ?)" (File "/baz.txt" (Just 100) Nothing)
  rs1 <- query conn "select * from file where size = ?" (Only (100 :: Integer)) :: IO [File]
  mapM_ (\(File path _ _) -> updateDigest conn path) rs1
  rs2 <- query conn "select * from file where digest = ?" (Only ("349a0426747b3b8c3583664a0ca66b6f" :: String)) :: IO [File]
  mapM_ (\(File path _ _) -> putStrLn path) rs2
  execute_ conn "drop table file"
  close conn
 where
  updateDigest conn path = execute conn "update file set digest = ? where path = ?" ("349a0426747b3b8c3583664a0ca66b6f" :: String, path)

