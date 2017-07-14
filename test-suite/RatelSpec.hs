module RatelSpec (spec) where

import qualified Control.Exception as Exception
import qualified Ratel
import           Test.Tasty.Hspec

spec :: Spec
spec = describe "Ratel" $ do
    describe "toError" $ do
        it "converts an Exception into an Error" $ do
            Exception.catch
                (do
                    _ <- error "something went wrong"
                    True `shouldBe` False)
                (\ exception -> do
                    let actual = Ratel.toError (exception :: Exception.SomeException)
                    let expected = Ratel.Error
                            { Ratel.errorBacktrace = Just
                                [ Ratel.Trace
                                    { Ratel.traceFile = Just "test-suite/RatelSpec.hs"
                                    , Ratel.traceMethod = Just "RatelSpec.toError"
                                    , Ratel.traceNumber = Just "16:34"
                                    }
                                ]
                            , Ratel.errorClass = Just "SomeException: something went wrong"
                            , Ratel.errorMessage = Just "\
                                \something went wrong\n\
                                \CallStack (from HasCallStack):\n\
                                \  error, called at test-suite/RatelSpec.hs:13:26 in main:RatelSpec"
                            , Ratel.errorSource = Nothing
                            , Ratel.errorTags = Nothing
                            }
                    actual `shouldBe` expected)
        it "should abbreviate longer errors" $ do
          Exception.catch
                (do
                    _ <- error "something went wrong, and now the server is on fire"
                    True `shouldBe` False)
                (\ exception -> do
                    let actual = Ratel.toError (exception :: Exception.SomeException)
                    let expected = Ratel.Error
                            { Ratel.errorBacktrace = Just
                                [ Ratel.Trace
                                    { Ratel.traceFile = Just "test-suite/RatelSpec.hs"
                                    , Ratel.traceMethod = Just "RatelSpec.toError"
                                    , Ratel.traceNumber = Just "40:34"
                                    }
                                ]
                            , Ratel.errorClass = Just "SomeException: something went wrong, and now "
                            , Ratel.errorMessage = Just "\
                                \something went wrong, and now the server is on fire\n\
                                \CallStack (from HasCallStack):\n\
                                \  error, called at test-suite/RatelSpec.hs:37:26 in main:RatelSpec"
                            , Ratel.errorSource = Nothing
                            , Ratel.errorTags = Nothing
                            }
                    actual `shouldBe` expected)
