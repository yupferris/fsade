import System.Environment
import System.Directory

data NewProjectInfo = NewProjectInfo
  {
    name :: String
  } deriving (Show)

data Operation =
  None
  | NewProject NewProjectInfo
  | Invalid

main = do
  stringArgs <- getArgs
  let operation = parseArgs stringArgs
  perform operation

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
  let directoryName = name info
  createDirectory directoryName
