{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sqlite ( (=.)
                               , (==.)
                               , Entity(..)
                               , Filter
                               , SelectOpt(Asc)
                               , deleteWhere
                               , insert_
                               , runMigrationSilent
                               , runSqlite
                               , selectList
                               , update
                               )
import Database.Persist.TH ( mkMigrate
                           , mkPersist
                           , persistLowerCase
                           , share
                           , sqlSettings
                           )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  File
    path String
    size Int Maybe
    digest String Maybe
    Path path
    Primary path
    deriving Show
|]


main :: IO ()
main = runSqlite "persistent-sqlite.sqlite3" $ do
  let dummySize = Just 100
  runMigrationSilent migrateAll

  insert_ $ File "/foo.txt" dummySize Nothing
  insert_ $ File "/bar.txt" Nothing Nothing
  insert_ $ File "/baz.txt" dummySize Nothing

  fileList <- selectList [FileSize ==. dummySize] [Asc FileId]
  mapM_ updateDigest fileList

  fileListWithDigest <- selectList [FileDigest ==. dummyDigest] [Asc FilePath]
  liftIO $ mapM_ printFilePath fileListWithDigest

  deleteWhere ([] :: [Filter File])
 where
  updateDigest (Entity key (File path _ _)) = do
    digest <- liftIO $ getDigest path
    update key [FileDigest =. digest]
  dummyDigest = Just "349a0426747b3b8c3583664a0ca66b6f"
  getDigest _ = return dummyDigest
  printFilePath (Entity _ (File path _ _)) = putStrLn path

