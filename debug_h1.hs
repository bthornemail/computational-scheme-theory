import ComputationalScheme.Algorithm1.Parser
import ComputationalScheme.Algorithm1.AST
import ComputationalScheme.Algorithm1.AlphaConversion
import ComputationalScheme.Algorithm2.Scope
import ComputationalScheme.Algorithm2.Topology
import ComputationalScheme.Algorithm3.CechComplex
import ComputationalScheme.Rig
import ComputationalScheme.Algorithm1.BindingExtractor
import Data.Text
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main = do
  let source = "(define (test x) (let ((a x)) (let ((b (+ a 1))) (+ a b))))"
  case parseProgram (pack source) of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right exprs -> do
      let converted = alphaConvert (head exprs)
      let scopeMap = analyzeScopes converted
      putStrLn $ "Bindings: " ++ show (Map.keys scopeMap)
      mapM_ (\(k, v) -> putStrLn $ "  " ++ show k ++ ": " ++ show v) (Map.toList scopeMap)
