import System.Environment
import System.Directory
import System.IO
import Solution

main = do
  stringArgs <- getArgs
  let operation = parseArgs stringArgs
  perform operation

data Operation = None
               | NewProject NewProjectInfo
               | Invalid

data NewProjectInfo = NewProjectInfo { name :: String
                                     } deriving (Show)

parseArgs [] = None
parseArgs ["new", arg] = NewProject NewProjectInfo { name = arg }
parseArgs _ = Invalid

perform None = printHelp
perform (NewProject info) = newProject info
perform Invalid = do
  putStrLn "Invalid arguments specified"
  printHelp

printHelp = do
  putStrLn "fsade: A simple tool for managing F# projects"

  putStrLn ""

  putStrLn "Usage:"
  putStrLn "\tfsade <command> [<args>...]"

  putStrLn ""

  putStrLn "Commands:"
  putStrLn "\tnew <name>\tCreate a new F# project called <name>"

newProject info = do
  -- TODO: Not entirely convinced this functionality should be top-level;
  -- for instance, the knowledge that a solution exists in a folder (and
  -- soon that projects may exist in subfolders) seems like it should live
  -- in the solution/project module(s).
  let solutionDirectory = name info
  createDirectory solutionDirectory

  let sln = createDefaultSolution $ name info
  let slnFilePath = solutionDirectory ++ "/" ++ name info ++ ".sln"
  serializeSolutionFile slnFilePath sln
