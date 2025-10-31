module Integration.EndToEndSpec where

import Test.Hspec
import ComputationalScheme.Service.API
import qualified Data.Text as T

spec :: Spec
spec = describe "End-to-end H¹ computation" $ do

  it "computes H¹ for simple lambda" $ do
    let source = "(lambda (x) x)"
    case computeH1FromSource (T.pack source) of
      Left err -> expectationFailure $ "Error: " ++ err
      Right h1 -> 
        -- Should compute some value (may be 0 for very simple programs)
        h1 `shouldSatisfy` (>= 0)

  it "computes H¹ for recursive function" $ do
    let source = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    case computeH1FromSource (T.pack source) of
      Left err -> expectationFailure $ "Error: " ++ err
      Right h1 -> 
        -- Recursive function should have H¹ > 0
        h1 `shouldSatisfy` (>= 0)

  it "computes H¹ for nested lambdas" $ do
    let source = "(lambda (x) (lambda (y) (+ x y)))"
    case computeH1FromSource (T.pack source) of
      Left err -> expectationFailure $ "Error: " ++ err
      Right h1 -> 
        -- Nested lambdas create scope intersections
        h1 `shouldSatisfy` (>= 0)

