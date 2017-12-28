{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BI
import Database.SQLite3.Direct ( Utf8(Utf8)
                               , close
                               , exec
                               , execWithCallback
                               , open
                               )
import Codec.Binary.UTF8.String ( encode
                                , decode
                                )

main :: IO ()
main = do
  edb <- open "direct-sqlite.sqlite3"
  case edb of
    Left (_, Utf8 msg) -> putStrLn $ decode (BI.unpack msg)
    Right db -> do
      _ <- exec db $ Utf8 "create table file (path text primary key, size integer, digest text)"
      _ <- exec db $ Utf8 "insert into file values ('/foo.txt', 100, null)"
      _ <- exec db $ Utf8 "insert into file values ('/bar.txt', null, null)"
      _ <- exec db $ Utf8 "insert into file values ('/baz.txt', 100, null)"
      _ <- execWithCallback db (Utf8 "select path from file where size = 100") (\_ _ [value] -> updateDigest db value)
      _ <- execWithCallback db (Utf8 "select path from file where digest = '349a0426747b3b8c3583664a0ca66b6f'") (\_ _ [value] -> print value)
      _ <- exec db $ Utf8 "delete from file"
      close db
      return ()
 where
  updateDigest _ Nothing = return ()
  updateDigest db (Just (Utf8 path)) = do
    _ <- exec db $ Utf8 (BI.pack $ encode ("update file set digest = '349a0426747b3b8c3583664a0ca66b6f' where path = '" ++ (decode $ BI.unpack path) ++ "'"))
    return ()

