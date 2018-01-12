import qualified Control.Exception as Exception
import qualified Ratel
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec . Hspec.describe "Ratel" $
  Hspec.describe "toError" $
    Hspec.it "converts an Exception into an Error" $
      Exception.catch
        (do
          _ <- Exception.throwIO (userError "something went wrong")
          True `Hspec.shouldBe` False)
        (\ exception -> do
          let
            file = FilePath.combine "test-suite" "Main.hs"
            actual = Ratel.toError (exception :: Exception.SomeException)
            expected = Ratel.Error
              { Ratel.errorBacktrace = Just
                [ Ratel.Trace
                  { Ratel.traceFile = Just file
                  , Ratel.traceMethod = Just "Main.toError"
                  , Ratel.traceNumber = Just "17:22"
                  }
                ]
              , Ratel.errorClass = Just "SomeException"
              , Ratel.errorMessage = Just "user error (something went wrong)"
              , Ratel.errorSource = Nothing
              , Ratel.errorTags = Nothing
              }
          actual `Hspec.shouldBe` expected)
