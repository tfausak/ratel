import qualified Control.Exception as Exception
import qualified Data.Maybe as Maybe
import qualified Ratel
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

main :: IO ()
main =
  Hspec.hspec
    . Hspec.describe "Ratel"
    . Hspec.describe "toError"
    . Hspec.it "converts an Exception into an Error"
    $ Exception.catch
      ( do
          _ <- Exception.throwIO (userError "something went wrong")
          True `Hspec.shouldBe` False
      )
      ( \exception -> do
          let file = FilePath.joinPath ["source", "test-suite", "Main.hs"]
              actual = Ratel.toError (exception :: Exception.SomeException)
              expected =
                Ratel.Error
                  { Ratel.errorBacktrace =
                      Just
                        [ Ratel.Trace
                            { Ratel.traceFile = Just file,
                              Ratel.traceMethod = Just "Main.toError",
                              Ratel.traceNumber = Just "20:24"
                            }
                        ],
                    Ratel.errorClass = Just "SomeException",
                    Ratel.errorMessage = Just "varies by GHC version",
                    Ratel.errorSource = Nothing,
                    Ratel.errorTags = Nothing
                  }
          Ratel.errorBacktrace actual `Hspec.shouldBe` Ratel.errorBacktrace expected
          Ratel.errorClass actual `Hspec.shouldBe` Ratel.errorClass expected
          Ratel.errorMessage actual `Hspec.shouldSatisfy` Maybe.isJust
          Ratel.errorSource actual `Hspec.shouldBe` Ratel.errorSource expected
          Ratel.errorTags actual `Hspec.shouldBe` Ratel.errorTags expected
      )
