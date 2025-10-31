module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn "computational-scheme-theory 0.1.0.0"
    ["--help"] -> putStrLn helpText
    _ -> putStrLn "Computational Scheme Theory - Mathematical Core\nRun with --help for usage"
  exitSuccess

helpText :: String
helpText = unlines
  [ "Computational Scheme Theory - Mathematical Core"
  , ""
  , "Usage:"
  , "  computational-scheme-theory [OPTIONS]"
  , ""
  , "Options:"
  , "  --help     Show this help message"
  , "  --version  Show version number"
  , ""
  , "This executable will be extended in Month 1 to support:"
  , "  - Parsing Scheme programs"
  , "  - Computing H¹ cohomology"
  , "  - Running as gRPC service"
  ]

