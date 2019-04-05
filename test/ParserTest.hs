module Main where

import           Data.Either            (isLeft)
import           System.CLI.Command     (Command (..))
import           System.CLI.Environment (RuntimeEnv (..))
import           System.CLI.Parser      (parseCommands)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  sucParserTests
  failParserTests

sucParserTests :: Spec
sucParserTests = describe "Check successful parsing." $ do
    it "many echo" $
        parseCommands "echo|echo|echo "
          `shouldBe` Right [ Echo [], Echo [], Echo []
                           ]
    it "external command" $
        parseCommands "echosss \"123\" "
          `shouldBe` Right [ ExternalCommand "echosss \"123\" "
                          ]

    it "echo/cat/external command/wc" $
        parseCommands "echo \"123\" | cat    | exec something -228 | cat \'here/comes\'| wc   "
          `shouldBe` Right [ Echo ["123"], Cat Nothing, ExternalCommand "exec something -228 "
                           , Cat (Just "here/comes"), Wc Nothing
                           ]

    it "echo with many parameters" $
      parseCommands "echo \"123\" abcd dasd22 | exec something \'here/comes\' \"it\""
        `shouldBe` Right [ Echo ["123", "abcd", "dasd22"], ExternalCommand "exec something \'here/comes\' \"it\""
                         ]

    it "wc with tricky argument" $
      parseCommands "wc \"path/to/interesting___file\" | echo | pwd | exit"
        `shouldBe` Right [ Wc (Just "path/to/interesting___file"), Echo [], Pwd, Exit
                         ]

    it "multiple assignments" $
      parseCommands "  AAA=288AAAC | CC234=111 | exit"
        `shouldBe` Right [ Assignment "AAA" "288AAAC", Assignment "CC234" "111", Exit
                         ]

failParserTests :: Spec
failParserTests = describe "Check unsuccessful parsing." $ do
    it "empty space between two |" $
      isLeft (parseCommands "echo \"123\" | cat |   | ") `shouldBe` True
    it "only spaces" $
      isLeft (parseCommands "    ") `shouldBe` True
    it "several commands in one part of pipeline" $
      isLeft (parseCommands "echo \"123\" | cat one echo ") `shouldBe` True
    it "echos with delimiter on end" $
        isLeft (parseCommands "echo|echo|echo |") `shouldBe` True
