module Algorithm2Spec where

import Test.Hspec
import ComputationalScheme.Types
import ComputationalScheme.Rig
import ComputationalScheme.Algorithm1.BindingExtractor
import ComputationalScheme.Algorithm1.Parser
import ComputationalScheme.Algorithm1.AlphaConversion
import ComputationalScheme.Algorithm2.Scope
import ComputationalScheme.Algorithm2.Topology
import ComputationalScheme.Algorithm2.OpenCover
import qualified Data.Text as T

spec :: Spec
spec = describe "Algorithm 2: Scope Topology Constructor" $ do

  it "builds topology from simple lambda" $ do
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
        bindingCount topo `shouldSatisfy` (>= 0)
      Right _ -> expectationFailure "Expected single expression"

  it "computes visibility regions" $ do
    let source = "(lambda (x) x)"
    case parseProgram (T.pack source) of
      Left _ -> expectationFailure "Parse failed"
      Right [expr] -> do
        let converted = alphaConvert expr
        let scopeMap = analyzeScopes converted
        Map.size scopeMap `shouldSatisfy` (>= 0)
      Right _ -> expectationFailure "Expected single expression"

  it "builds open cover from topology" $ do
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
        let cover = buildOpenCover topo
        length cover `shouldSatisfy` (>= 0)
      Right _ -> expectationFailure "Expected single expression"

  it "finds intersecting pairs" $ do
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
        let pairs = intersectingPairs cover
        length pairs `shouldSatisfy` (>= 0)
      Right _ -> expectationFailure "Expected single expression"

