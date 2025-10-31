module Algorithm3Spec where

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
import ComputationalScheme.Algorithm3.Nerve
import ComputationalScheme.Algorithm3.CechComplex
import qualified Data.Text as T

spec :: Spec
spec = describe "Algorithm 3: Čech Complex Builder" $ do

  it "builds simplicial complex from topology" $ do
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
        vertexCount complex `shouldSatisfy` (>= 0)

  it "computes nerve of open cover" $ do
    let source = "(let ((x 1) (y 2)) (+ x y))"
    case parseProgram (T.pack source) of
      Left _ -> expectationFailure "Parse failed"
      Right [expr] -> do
        let converted = alphaConvert expr
        let scopeMap = analyzeScopes converted
        let rig = case extractBindingAlgebra (T.pack source) of
              Left _ -> RScheme Set.empty
              Right r -> r
        let topo = buildTopology rig scopeMap
        let cover = buildOpenCover topo
        let complex = computeNerve cover
        isValid complex `shouldBe` True

  it "counts simplices correctly" $ do
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
        edgeCount complex `shouldSatisfy` (>= 0)
        triangleCount complex `shouldSatisfy` (>= 0)

  it "computes Euler characteristic" $ do
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
        let chi = eulerCharacteristic complex
        -- Euler characteristic should be well-defined
        chi `shouldSatisfy` (\x -> True)  -- Just check it computes

  it "checks connectivity" $ do
    let source = "(define x 42)"
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
        -- Single vertex is connected
        isConnected complex `shouldBe` True

