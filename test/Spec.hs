import Language

import Test.Hspec

main :: IO ()
main = hspec $
    describe "pprProgram" $
      it "converts a CoreProgram to an Iseq" $ do
        let iseq =
              IAppend
                (IAppend
                   (IAppend (IAppend (IStr "I") (IStr " ")) (IStr "x"))
                   (IStr " = "))
                (IStr "x")
        let prog = pprProgram [("I", ["x"], EVar "x")]
        prog `shouldBe` iseq
