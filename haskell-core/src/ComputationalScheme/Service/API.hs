{-# LANGUAGE OverloadedStrings #-}

-- | High-Level API for Computing H¹ from Scheme Source
--
-- This module provides the main entry point for the complete pipeline:
-- Scheme source → H¹ cohomology
module ComputationalScheme.Service.API where

import ComputationalScheme.Types
import ComputationalScheme.Rig
import ComputationalScheme.Algorithm1.BindingExtractor
import ComputationalScheme.Algorithm1.Parser
import ComputationalScheme.Algorithm1.AlphaConversion
import ComputationalScheme.Algorithm2.Scope
import ComputationalScheme.Algorithm2.ScopeAnalysis
import ComputationalScheme.Algorithm2.Topology
import ComputationalScheme.Algorithm2.OpenCover
import ComputationalScheme.Algorithm3.CechComplex
import ComputationalScheme.Algorithm3.SimplicialComplex
import ComputationalScheme.Algorithm4.Cohomology hiding (H1Result(..))
import ComputationalScheme.Algorithm4.ChainComplex (CohomologyGroup(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

-- | Complete pipeline result
data PipelineResult = PipelineResult
  { h1Value :: Int
  , beta0 :: Int
  , beta1 :: Int
  , numBindings :: Int
  , numSimplices0 :: Int
  , numSimplices1 :: Int
  , numSimplices2 :: Int
  , success :: Bool
  , errorMessage :: Maybe String
  }
  deriving (Eq, Show)

-- | Compute H¹ from Scheme source code
-- This is the main entry point that runs the complete pipeline:
--   1. Parse source → AST
--   2. Extract binding algebra (R_Scheme)
--   3. Build topology
--   4. Construct Čech complex
--   5. Compute H¹
computeH1FromSource :: Text -> Either String Int
computeH1FromSource source = do
  -- Step 1: Parse
  exprs <- case parseProgram source of
    Left err -> Left $ "Parse error: " ++ show err
    Right es -> Right es
  
  -- Step 2: Extract bindings
  rig <- case extractBindingAlgebra source of
    Left err -> Left err
    Right r -> Right r
  
  -- Step 3: Analyze scopes with enhanced tree-based analysis
  let convertedExpr = alphaConvert (head exprs)
  let (enhancedScopeMap, scopeTree) = analyzeScopesEnhanced convertedExpr
  
  -- Step 4: Build topology (using enhanced scope analysis)
  let topo = buildTopologyEnhanced rig enhancedScopeMap scopeTree
  
  -- Step 5: Build Čech complex
  let complex = buildCechComplex topo
  
  -- Step 6: Compute H¹
  let CohomologyGroup h1 = computeH1 complex
  
  return h1

-- | Compute full pipeline result with detailed information
computeH1FromSourceDetailed :: Text -> PipelineResult
computeH1FromSourceDetailed source = 
  case computeH1FromSource source of
    Left err -> PipelineResult
      { h1Value = 0
      , beta0 = 0
      , beta1 = 0
      , numBindings = 0
      , numSimplices0 = 0
      , numSimplices1 = 0
      , numSimplices2 = 0
      , success = False
      , errorMessage = Just err
      }
    Right h1 -> 
      -- Extract additional statistics
      case extractBindingAlgebra source of
        Left err -> PipelineResult
          { h1Value = h1
          , beta0 = 0
          , beta1 = h1
          , numBindings = 0
          , numSimplices0 = 0
          , numSimplices1 = 0
          , numSimplices2 = 0
          , success = True
          , errorMessage = Just err
          }
        Right rig -> 
          let numBindings = ComputationalScheme.Rig.bindingCount rig
              exprs = case parseProgram source of
                Left _ -> []
                Right es -> es
              convertedExpr = if null exprs then undefined else alphaConvert (head exprs)
              (enhancedScopeMap, scopeTree) = analyzeScopesEnhanced convertedExpr
              topo = buildTopologyEnhanced rig enhancedScopeMap scopeTree
              complex = buildCechComplex topo
          in PipelineResult
            { h1Value = h1  -- PipelineResult.h1Value
            , beta0 = 1  -- Simplified
            , beta1 = h1
            , numBindings = numBindings
            , numSimplices0 = Set.size (simplices0 complex)
            , numSimplices1 = Set.size (simplices1 complex)
            , numSimplices2 = Set.size (simplices2 complex)
            , success = True
            , errorMessage = Nothing
            }

