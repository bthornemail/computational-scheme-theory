module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import ComputationalScheme.Service.API
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn "computational-scheme-theory 0.1.0.0"
    ["--help"] -> putStrLn helpText
    ["compute-h1", file] -> computeH1FromFile file
    ["--demo"] -> runDemo
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

