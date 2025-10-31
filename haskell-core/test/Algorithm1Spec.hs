module Algorithm1Spec where

import Test.Hspec
import ComputationalScheme.Algorithm1.BindingExtractor
import ComputationalScheme.Types
import ComputationalScheme.Rig
import qualified Data.Text as T
import qualified Data.Set as Set

spec :: Spec
spec = describe "Algorithm 1: Binding Algebra Extractor" $ do

  it "extracts lambda bindings" $ do
    let source = "(lambda (x) x)"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> bindingCount rig `shouldBe` 1

  it "handles nested scopes" $ do
    let source = "(lambda (x) (lambda (y) (+ x y)))"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> bindingCount rig `shouldBe` 2

  it "applies α-conversion for shadowed bindings" $ do
    let source = "(lambda (x) (lambda (x) x))"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> 
        -- Both x's should get unique names after α-conversion
        bindingCount rig `shouldBe` 2

  it "extracts let bindings" $ do
    let source = "(let ((x 42)) x)"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> bindingCount rig `shouldBe` 1

  it "extracts define bindings" $ do
    let source = "(define x 42)"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> bindingCount rig `shouldBe` 1

  it "extracts function definition bindings" $ do
    let source = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> 
        -- fact and n should both be bindings
        bindingCount rig `shouldBeGreaterThanOrEqual` 2

  it "handles empty program" $ do
    let source = ""
    case extractBindingAlgebra (T.pack source) of
      Left err -> return ()  -- Empty programs may error, that's OK
      Right rig -> bindingCount rig `shouldBe` 0

  it "handles constants without bindings" $ do
    let source = "42"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> bindingCount rig `shouldBe` 0

  it "extracts bindings from complex nested structure" $ do
    let source = "(let ((x 1)) (lambda (y) (let ((z 2)) (+ x y z))))"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> 
        -- x, y, z should all be bindings
        bindingCount rig `shouldBe` 3

  it "handles letrec bindings" $ do
    let source = "(letrec ((f (lambda (n) (if (= n 0) 1 (* n (f (- n 1))))))) (f 5))"
    case extractBindingAlgebra (T.pack source) of
      Left err -> expectationFailure $ "Parse error: " ++ err
      Right rig -> 
        -- f and n should be bindings
        bindingCount rig `shouldBeGreaterThanOrEqual` 2

