{-# LANGUAGE OverloadedStrings #-}

-- | Simple test executable for scope tree analysis
module Main where

import ComputationalScheme.Algorithm1.Parser (parseProgram)
import ComputationalScheme.Algorithm2.ScopeAnalysis (analyzeScopesEnhanced)
import ComputationalScheme.Algorithm2.ScopeTree
import ComputationalScheme.Types
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn "=== Testing Scope Tree Analysis ===\n"
  
  -- Test 1: Simple function with parameters
  putStrLn "Test 1: Simple function with 2 parameters"
  testProgram "(define (add x y) (+ x y))"
  
  -- Test 2: Function with if statement  
  putStrLn "\nTest 2: Function with if statement"
  testProgram "(define (abs x) (if (< x 0) (- x) x))"
  
  putStrLn "\n=== All Tests Complete ==="

testProgram :: T.Text -> IO ()
testProgram source = do
  putStrLn $ "Source: " ++ T.unpack source
  putStrLn ""
  
  case parseProgram source of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
    Right exprs -> do
      case exprs of
        [expr] -> do
          let (bindings, tree) = analyzeScopesEnhanced expr
          
          putStrLn $ "Scope Tree Nodes: " ++ show (Map.size (nodes tree))
          putStrLn $ "Bindings Found: " ++ show (Map.size bindings)
          putStrLn ""
          
          -- Print scope tree structure
          putStrLn "Scope Tree Structure:"
          mapM_ printScopeNode (Map.toList (nodes tree))
          putStrLn ""
          
          -- Print bindings and their scope assignments
          putStrLn "Bindings:"
          mapM_ (printBinding tree) (Map.toList bindings)
          putStrLn ""
          
          -- Find overlapping pairs
          let overlaps = findOverlappingPairs tree
          putStrLn $ "Overlapping Scope Pairs: " ++ show (length overlaps)
          if not (null overlaps)
            then mapM_ (\(s1, s2) -> putStrLn $ "  " ++ show s1 ++ " <-> " ++ show s2) overlaps
            else putStrLn "  (none)"
          putStrLn ""
          
        _ -> do
          putStrLn "Expected single expression, got multiple"
  
  putStrLn $ replicate 60 '-'

printScopeNode :: (ScopeId, ScopeTreeNode) -> IO ()
printScopeNode (sid, node) = do
  putStrLn $ "  Scope " ++ show sid ++ ":"
  putStrLn $ "    Type: " ++ show (scopeType node)
  putStrLn $ "    Depth: " ++ show (scopeDepth node)
  putStrLn $ "    Parent: " ++ show (parent node)
  putStrLn $ "    Children: " ++ show (length (children node))
  putStrLn $ "    Bindings: " ++ show (Set.size (scopeBindings node))

printBinding :: ScopeTree -> (BindingId, EnhancedVisibilityRegion) -> IO ()
printBinding tree (bid, region) = do
  putStrLn $ "  Binding: " ++ show bid
  putStrLn $ "    Scope IDs: " ++ show (Set.toList (scopeIds region))
  putStrLn $ "    Usage Locations: " ++ show (length (usageLocations (usagePattern region)))
  putStrLn $ "    Usage Contexts: " ++ show (usageContexts (usagePattern region))
  let depths = map (getScopeDepth tree) (Set.toList (scopeIds region))
  putStrLn $ "    Scope Depths: " ++ show depths

