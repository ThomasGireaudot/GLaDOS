import Running
import Datatypes (createReferenceSet, createLambdaSet)
-- import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = runPrompt createReferenceSet createLambdaSet
