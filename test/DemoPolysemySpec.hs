module DemoPolysemySpec where

import Test.HUnit
import Test.Hspec
import DemoPolysemy
import Polysemy
import Polysemy.Error

spec :: Spec
spec = do
  describe "Error Effect" $ do
    it "can throw error" $ do
      let n = run . runError $ f2
      n @?= Left "Error"
    
    it "catches the error" $ do
      let m = run . runError $ f
      m @?= Right "Caught"