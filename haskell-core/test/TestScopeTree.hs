{-# LANGUAGE OverloadedStrings #-}

-- | Test the new scope tree analysis module
module Main (main) where

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
  test1 <- testProgram
    "(define (add x y) (+ x y))"
    "Simple function with 2 parameters"
  
  -- Test 2: Nested lets
  test2 <- testProgram
    "(define (test) (let ((a 1)) (let ((b 2)) (+ a b))))"
    "Nested let bindings"
  
  -- Test 3: Function with if statement
  test3 <- testProgram
    "(define (abs x) (if (< x 0) (- x) x))"
    "Function with if statement"
  
  -- Test 4: Recursive function
  test4 <- testProgram
    "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
    "Recursive function"
  
  putStrLn "\n=== All Tests Complete ==="

testProgram :: T.Text -> String -> IO ()
testProgram source desc = do
  putStrLn $ "Test: " ++ desc
  putStrLn $ "Source: " ++ T.unpack source
  putStrLn ""
  
  case parseProgram source of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      putStrLn ""
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
          putStrLn "Bindings and Scopes:"
          mapM_ (printBinding tree) (Map.toList bindings)
          putStrLn ""
          
          -- Find overlapping pairs
          let overlaps = findOverlappingPairs tree
          putStrLn $ "Overlapping Scope Pairs: " ++ show (length overlaps)
          mapM_ (\(s1, s2) -> putStrLn $ "  " ++ show s1 ++ " <-> " ++ show s2) overlaps
          putStrLn ""
          
        _ -> do
          putStrLn "Expected single expression, got multiple"
          putStrLn ""
  
  putStrLn $ replicate 60 '-'
  putStrLn ""

printScopeNode :: (ScopeId, ScopeTreeNode) -> IO ()
printScopeNode (sid, node) = do
  putStrLn $ "  Scope " ++ show sid ++ ":"
  putStrLn $ "    Type: " ++ show (scopeType node)
  putStrLn $ "    Depth: " ++ show (scopeDepth node)
  putStrLn $ "    Parent: " ++ show (parent node)
  putStrLn $ "    Children: " ++ show (children node)
  putStrLn $ "    Bindings: " ++ show (Set.size (scopeBindings node))

printBinding :: ScopeTree -> (BindingId, EnhancedVisibilityRegion) -> IO ()
printBinding tree (bid, region) = do
  putStrLn $ "  Binding: " ++ show bid
  putStrLn $ "    Scope IDs: " ++ show (Set.toList (scopeIds region))
  putStrLn $ "    Usage Locations: " ++ show (length (usageLocations (usagePattern region)))
  putStrLn $ "    Usage Contexts: " ++ show (usageContexts (usagePattern region))
  -- Show scope depths
  let depths = map (getScopeDepth tree) (Set.toList (scopeIds region))
  putStrLn $ "    Scope Depths: " ++ show depths
  putStrLn ""

