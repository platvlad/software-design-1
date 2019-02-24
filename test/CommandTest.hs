{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString        (ByteString)
import           Data.Either            (isLeft)
import qualified Data.Map.Strict        as M (fromList)
import           System.CLI.Command     (Command (..), executePipeline)
import           System.CLI.Environment (RuntimeEnv (..), runCLIMonad, toStream)
import           System.Directory       (getCurrentDirectory)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  singleCommands
  pipelinedCommands

singleCommands :: Spec
singleCommands = describe "Check commands by themselves." $ do
    it "echo" $ do
        res <- fmap stream <$> (liftIO $ runCLIMonad $ executePipeline [Echo ["123", "aaaar dssd ", "____\'\'"]])
        res `shouldBe` Right "123 aaaar dssd  ____\'\'"
    it "pwd" $ do
        res <- fmap stream <$> (liftIO $ runCLIMonad $ executePipeline [Pwd])
        cwd <- liftIO getCurrentDirectory
        res `shouldBe` Right (toStream cwd)
    it "wc" $ do
        cwd <- liftIO getCurrentDirectory
        res <- fmap stream <$> (liftIO $ runCLIMonad $ executePipeline [Wc $ Just $ cwd ++ "/" ++ "test/data/LICENSE"])
        res `shouldBe` Right "29 226 1509"
    it "exit" $ do
        res <- fmap exit <$> (liftIO $ runCLIMonad $ executePipeline [Exit])
        res `shouldBe` Right True
    it "assignment" $ do
        res <- fmap varState <$> (liftIO $ runCLIMonad $ executePipeline [Assignment "AAA" "288AAAC"])
        res `shouldBe` Right (M.fromList [("AAA", "288AAAC")])

pipelinedCommands :: Spec
pipelinedCommands = describe "Check commands linked with pipes." $ do
    it "echo + wc" $ do
        res <- fmap stream <$> (liftIO $ runCLIMonad $ executePipeline [Echo ["123", "aaaar dssd ", "____\'\'"], Wc Nothing])
        res `shouldBe` Right "1 4 22"
    it "cat + wc" $ do
        cwd <- liftIO getCurrentDirectory
        res <- fmap stream <$> (liftIO $ runCLIMonad $ executePipeline [Cat $ Just $ cwd ++ "/" ++ "test/data/LICENSE", Wc Nothing])
        res `shouldBe` Right "29 226 1509"
    it "echo + assignment + exit" $ do
        res <- liftIO $ runCLIMonad $ executePipeline [Echo ["123", "aaaar dssd ", "____\'\'"], Assignment "AAA" "288AAAC", Exit]
        fmap stream res `shouldBe` Right ""
        fmap varState res `shouldBe` Right (M.fromList [("AAA", "288AAAC")])
        fmap exit res `shouldBe` Right True
