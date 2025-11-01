module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import ComputationalScheme.Service.API
import ComputationalScheme.Algorithm1.Parser (parseProgram)
import ComputationalScheme.Algorithm2.ScopeAnalysis (analyzeScopesEnhanced)
import ComputationalScheme.Algorithm2.ScopeTree
import ComputationalScheme.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn "computational-scheme-theory 0.1.0.0"
    ["--help"] -> putStrLn helpText
    ["compute-h1", file] -> computeH1FromFile file
    ["--demo"] -> runDemo
    ["test-scope-tree", file] -> testScopeTree file
    ["test-scope-tree"] -> testScopeTreeDefault
    _ -> do
      putStrLn "Computational Scheme Theory - Mathematical Core"
      putStrLn "Run with --help for usage"
      exitSuccess

helpText :: String
helpText = unlines
  [ "Computational Scheme Theory - Mathematical Core"
  , ""
  , "Usage:"
  , "  computational-scheme-theory [COMMAND] [OPTIONS]"
  , ""
  , "Commands:"
  , "  compute-h1 <file>    Compute H¹ cohomology for Scheme program"
  , "  test-scope-tree [file]  Test scope tree analysis (default test programs if no file)"
  , "  --demo               Run demonstration"
  , ""
  , "Options:"
  , "  --help               Show this help message"
  , "  --version            Show version number"
  , ""
  , "Examples:"
  , "  computational-scheme-theory compute-h1 program.scm"
  , "  computational-scheme-theory --demo"
  ]

computeH1FromFile :: FilePath -> IO ()
computeH1FromFile file = do
  source <- TIO.readFile file
  case computeH1FromSource source of
    Left err -> do
      putStrLn $ "Error: " ++ err
      exitFailure
    Right h1 -> do
      putStrLn $ "H¹(X_Comp, O_Comp) = " ++ show h1
      exitSuccess

runDemo :: IO ()
runDemo = do
  let demoProgram = T.pack "(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))"
  putStrLn "Computational Scheme Theory - Demo"
  putStrLn ""
  putStrLn "Program:"
  TIO.putStrLn demoProgram
  putStrLn ""
  putStrLn "Computing H¹..."
  case computeH1FromSource demoProgram of
    Left err -> putStrLn $ "Error: " ++ err
    Right h1 -> putStrLn $ "H¹ = " ++ show h1
  exitSuccess

testScopeTree :: FilePath -> IO ()
testScopeTree file = do
  source <- TIO.readFile file
  putStrLn $ "=== Testing Scope Tree Analysis ==="
  putStrLn $ "File: " ++ file
  putStrLn $ "Source:\n" ++ T.unpack source
  putStrLn ""
  
  case parseProgram source of
    Left err -> do
      putStrLn $ "Parse error: " ++ show err
      exitFailure
    Right exprs -> do
      case exprs of
        [expr] -> do
          let (bindings, tree) = analyzeScopesEnhanced expr
          
          putStrLn $ "Scope Tree Nodes: " ++ show (Map.size (nodes tree))
          putStrLn $ "Bindings Found: " ++ show (Map.size bindings)
          putStrLn ""
          
          putStrLn "Scope Tree Structure:"
          mapM_ printScopeNode (Map.toList (nodes tree))
          putStrLn ""
          
          putStrLn "Bindings:"
          mapM_ (printBinding tree) (Map.toList bindings)
          putStrLn ""
          
          let overlaps = findOverlappingPairs tree
          putStrLn $ "Overlapping Scope Pairs: " ++ show (length overlaps)
          if not (null overlaps)
            then mapM_ (\(s1, s2) -> putStrLn $ "  " ++ show s1 ++ " <-> " ++ show s2) overlaps
            else putStrLn "  (none)"
          
          exitSuccess
        _ -> do
          putStrLn "Expected single expression, got multiple"
          exitFailure

testScopeTreeDefault :: IO ()
testScopeTreeDefault = do
  putStrLn "=== Testing Scope Tree Analysis (Default Tests) ===\n"
  
  testProgram (T.pack "(define (add x y) (+ x y))") "Simple function with 2 parameters"
  testProgram (T.pack "(define (abs x) (if (< x 0) (- x) x))") "Function with if statement"
  
  putStrLn "\n=== All Tests Complete ==="
  exitSuccess
  where
    testProgram :: Text -> String -> IO ()
    testProgram source desc = do
      putStrLn $ "Test: " ++ desc
      putStrLn $ "Source: " ++ T.unpack source
      putStrLn ""
      
      case parseProgram source of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right exprs -> do
          case exprs of
            [expr] -> do
              let (bindings, tree) = analyzeScopesEnhanced expr
              putStrLn $ "  Nodes: " ++ show (Map.size (nodes tree)) ++ 
                        ", Bindings: " ++ show (Map.size bindings)
              let overlaps = findOverlappingPairs tree
              putStrLn $ "  Overlaps: " ++ show (length overlaps)
            _ -> putStrLn "  Expected single expression"
      putStrLn $ replicate 60 '-'
      putStrLn ""

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

