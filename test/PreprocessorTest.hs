module Main where

import qualified Data.Map.Strict         as M (fromList)
import           System.CLI.Environment  (RuntimeEnv (..))
import           System.CLI.Preprocessor (preprocessCommands)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  sucPreprocessorTests
  failPreprocessorTests

sucPreprocessorTests :: Spec
sucPreprocessorTests = describe "Check successful preprocessing." $ do
    let re = RuntimeEnv (M.fromList [("aaa", "345"), ("bbb22", "fbdn-idqb"), ("sdsdd7b8", "\"  echo 23 4\"")]) mempty False

    it "two variables" $
      preprocessCommands re "echo $aaa | cat $sdsdd7b8 ||| $aaa" `shouldBe` Right "echo 345 | cat \"  echo 23 4\" ||| 345"

    it "many variables with \" symbols" $
      preprocessCommands re "echo $sdsdd7b8 | cat echi ||aa| $sdsdd7b8 $bbb22  " `shouldBe` Right "echo \"  echo 23 4\" | cat echi ||aa| \"  echo 23 4\" fbdn-idqb  "

    it "no variables" $
      preprocessCommands re "2  " `shouldBe` Right "2  "

    it "variable connected to different word" $
      preprocessCommands re "    wc $bbb22  | cat 345$bbb22" `shouldBe` Right "    wc fbdn-idqb  | cat 345fbdn-idqb"

failPreprocessorTests :: Spec
failPreprocessorTests = describe "Check unsuccessful preprocessing." $ do
    let re = RuntimeEnv (M.fromList [("aaa", "345"), ("bbb22", "fbdn-idqb"), ("sdsdd7b8", "\"  echo 23 4\"")]) mempty False

    it "unknown variables" $ do
        preprocessCommands re "echo $aaa | cat $sdsdd7bs8 ||| $aaa" `shouldBe` Left "Can't find variable with name sdsdd7bs8 in the environment."
        preprocessCommands re "echo $sdsdd7b8 | cat echi ||aa| $sdsdd7b8 $bbb222  " `shouldBe` Left "Can't find variable with name bbb222 in the environment."
        preprocessCommands re "    wc $22  | cat 345$bbb22" `shouldBe` Left "Can't find variable with name 22 in the environment."

    it "variable name with '$'" $ do
        preprocessCommands re "echo $aaa | cat $sdsdd7b8 ||$a$x| $aaa" `shouldBe` Left "Name of variable a$x contains can't contain '$'."
