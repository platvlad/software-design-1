{-# LANGUAGE OverloadedStrings #-}

module System.CLI.Command.Internal.Executor
  ( executeCommand
  ) where

import           Control.Exception                           (IOException, try)
import           Control.Monad.Except                        (throwError)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.State.Strict                  (get, modify)
import           System.CLI.Command.Internal.ExternalCommand (externalCommand)
import           System.CLI.Command.Internal.Types           (Command (..))
import           System.CLI.Command.Internal.Wc              (wcCommand)
import           System.CLI.Environment                      (CLIMonad,
                                                              RuntimeEnv (..),
                                                              Stream, insertVar,
                                                              intercalateStream,
                                                              readStream,
                                                              setExit,
                                                              setStream,
                                                              toStream)
import           System.Directory                            (getCurrentDirectory,
                                                              listDirectory,
                                                              setCurrentDirectory,
                                                              doesDirectoryExist)
import           Data.List                                   (intercalate)
-- | Execute given 'Command' according to its semantics.
--
executeCommand :: Command -> CLIMonad ()
executeCommand command = do
    re <- get

    case command of
      Cat (Just path)       -> safeReadFile path >>= modify . setStream
      Cat Nothing           -> pure ()
      Echo args             -> modify $ setStream $ intercalateStream " " (fmap toStream args)
      Wc pathM              -> do
          input <- maybe (pure $ stream re) safeReadFile pathM

          let (lc, wc, bc) = wcCommand input
          let stdout       = intercalateStream " " $ toStream . show <$> [lc, wc, bc]

          modify (setStream stdout)
      Pwd                   -> liftIO (fmap toStream getCurrentDirectory) >>= modify . setStream
      Exit                  -> modify (setExit True) >> modify (setStream "")
      Assignment var val    -> modify (insertVar var val) >> modify (setStream "")
      ExternalCommand shell -> externalCommand shell
      Cd (Just path)        -> liftIO (setDirectoryIfExists path) >> modify (setStream "")
      Cd Nothing            -> pure ()
      Ls (Just path)        -> lsDir path >>= getStreamFromList >>= modify . setStream
      Ls Nothing            -> lsCurrentDir >>= getStreamFromList >>= modify . setStream
  where
    lsDir :: FilePath -> CLIMonad [FilePath]
    lsDir path = liftIO (doesDirectoryExist path >>= (\x -> if (x)
                                                            then
                                                            listDirectory path
                                                            else pure []))
    lsCurrentDir :: CLIMonad [FilePath]
    lsCurrentDir = liftIO (getCurrentDirectory >>= listDirectory)
    getStreamFromList :: [FilePath] -> CLIMonad Stream
    getStreamFromList lst = pure (toStream (intercalate "\n" lst))
    setDirectoryIfExists :: FilePath -> IO ()
    setDirectoryIfExists path = doesDirectoryExist path >>= (setDirectoryOnCondition path)
    setDirectoryOnCondition :: FilePath -> Bool -> IO ()
    setDirectoryOnCondition path cond | cond = setCurrentDirectory path
                                      | otherwise = pure()
    safeReadFile :: String -> CLIMonad Stream
    safeReadFile file = do
      resE <- liftIO $ (try (readStream file) :: IO (Either IOException Stream))
      either (throwError . show) pure resE


