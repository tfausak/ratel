import qualified RatelSpec
import qualified Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "ratel" (parallel RatelSpec.spec)
    Test.Tasty.defaultMain test
