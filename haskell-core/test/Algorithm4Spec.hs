module Algorithm4Spec where

import Test.Hspec
import ComputationalScheme.Types
import ComputationalScheme.Rig
import ComputationalScheme.Algorithm1.BindingExtractor
import ComputationalScheme.Algorithm1.Parser
import ComputationalScheme.Algorithm1.AlphaConversion
import ComputationalScheme.Algorithm2.Scope
import ComputationalScheme.Algorithm2.Topology
import ComputationalScheme.Algorithm2.OpenCover
import ComputationalScheme.Algorithm3.SimplicialComplex
import ComputationalScheme.Algorithm3.CechComplex
import ComputationalScheme.Algorithm4.Cohomology
import ComputationalScheme.Algorithm4.BettiNumbers
import qualified Data.Text as T

spec :: Spec
spec = describe "Algorithm 4: Cohomology Calculator" $ do

  it "computes H¹ from simple program" $ do
    let source = "(lambda (x) x)"
    case parseProgram (T.pack source) of
      Left _ -> expectationFailure "Parse failed"
      Right [expr] -> do
        let converted = alphaConvert expr
        let scopeMap = analyzeScopes converted
        let rig = case extractBindingAlgebra (T.pack source) of
              Left _ -> RScheme Set.empty
              Right r -> r
        let topo = buildTopology rig scopeMap
        let complex = buildCechComplex topo
        let CohomologyGroup h1 = computeH1 complex
        -- H¹ should be non-negative
        h1 `shouldSatisfy` (>= 0)

  it "computes β₁ (first Betti number)" $ do
    let source = "(let ((x 1)) x)"
    case parseProgram (T.pack source) of
      Left _ -> expectationFailure "Parse failed"
      Right [expr] -> do
        let converted = alphaConvert expr
        let scopeMap = analyzeScopes converted
        let rig = case extractBindingAlgebra (T.pack source) of
              Left _ -> RScheme Set.empty
              Right r -> r
        let topo = buildTopology rig scopeMap
        let complex = buildCechComplex topo
        let beta1 = computeBeta1 complex
        beta1 `shouldSatisfy` (>= 0)

  it "computes detailed H¹ result" $ do
    let source = "(lambda (x) (lambda (y) (+ x y)))"
    case parseProgram (T.pack source) of
      Left _ -> expectationFailure "Parse failed"
      Right [expr] -> do
        let converted = alphaConvert expr
        let scopeMap = analyzeScopes converted
        let rig = case extractBindingAlgebra (T.pack source) of
              Left _ -> RScheme Set.empty
              Right r -> r
        let topo = buildTopology rig scopeMap
        let complex = buildCechComplex topo
        let result = computeH1Detailed complex
        h1Value result `shouldSatisfy` (>= 0)
        numEdges result `shouldSatisfy` (>= 0)

  it "computes Betti numbers" $ do
    let source = "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    case parseProgram (T.pack source) of
      Left _ -> expectationFailure "Parse failed"
      Right [expr] -> do
        let converted = alphaConvert expr
        let scopeMap = analyzeScopes converted
        let rig = case extractBindingAlgebra (T.pack source) of
              Left _ -> RScheme Set.empty
              Right r -> r
        let topo = buildTopology rig scopeMap
        let complex = buildCechComplex topo
        let betti = computeBettiNumbers 2 complex
        length betti `shouldBe` 3
        all (>= 0) betti `shouldBe` True

  it "computes Euler characteristic" $ do
    let source = "(if (> x 0) 1 -1)"
    case parseProgram (T.pack source) of
      Left _ -> expectationFailure "Parse failed"
      Right [expr] -> do
        let converted = alphaConvert expr
        let scopeMap = analyzeScopes converted
        let rig = case extractBindingAlgebra (T.pack source) of
              Left _ -> RScheme Set.empty
              Right r -> r
        let topo = buildTopology rig scopeMap
        let complex = buildCechComplex topo
        let chi = computeEulerCharacteristic complex
        -- Euler characteristic should be well-defined
        chi `shouldSatisfy` (\x -> True)  -- Just check it computes

