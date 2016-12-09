{-# LANGUAGE QuasiQuotes #-}
module ParseBuilds where

import Text.RawString.QQ
import Text.Parsec
import Parser

import Test.Hspec

simpleOneBuild :: String
simpleOneBuild
  = [r|
Build:
build
Run:
run
|]

simpleNoBuild :: String
simpleNoBuild
  = [r|
Build:
Run:
run
|]

indented :: String
indented
  = [r|
Build:
 build1
 build2
Run:
 run
|]

commented1 :: String
commented1
  = [r|
# Comment here
Build:
 build
Run:
 run
|]

commented2 :: String
commented2
  = [r|
# Comment here
Build:
# Comment here
 build
# Comment here
Run:
# Comment here
 run
# Comment here
|]

commented3 :: String
commented3
  = [r|
# Comment here #
Build: # comment
# Comment here
 build # comment
# Comment here
Run: # comment
# Comment here
 run # comment
# Comment here
|]

doParseBuild :: String -> Either ParseError ([String], [String])
doParseBuild
  = runParser parseBuildFile () ""

parseSpec :: SpecWith ()
parseSpec
  = describe "Build File Parser" $ do
      it "no build command, no indent" $
          doParseBuild simpleNoBuild
            `shouldBe` Right ([], ["run"])
      it "one build command, no indent" $
          doParseBuild simpleOneBuild
            `shouldBe` Right (["build"], ["run"])
      it "indented commands" $
          doParseBuild indented
            `shouldBe` Right (["build1", "build2"], ["run"])
      it "Simple line comments" $
          doParseBuild commented1
            `shouldBe` Right (["build"], ["run"])
      it "Complicated line comments" $
          doParseBuild commented2
            `shouldBe` Right (["build"], ["run"])
      it "Complicated line and end of line comments" $
          doParseBuild commented3
            `shouldBe` Right (["build"], ["run"])
