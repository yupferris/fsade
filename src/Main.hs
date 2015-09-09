import System.Environment
import System.Directory
import System.IO
import Project
import Solution
import Text.XML.Light -- TODO: Remove when project files are moved

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
  --  for instance, the knowledge that a solution exists in a folder (and
  --  soon that projects may exist in subfolders) seems like it should live
  --  in the solution/project module(s).
  let solutionDirectory = name info
  createDirectory solutionDirectory

  let localProjDirectory = name info
  let absoluteProjDirectory = solutionDirectory ++ "/" ++ localProjDirectory
  createDirectory absoluteProjDirectory

  proj <- createDefaultProject $ name info
  let projFileName = name info ++ ".fsproj"
  let localProjFilePath = localProjDirectory ++ "/" ++ projFileName
  let absoluteProjFilePath = absoluteProjDirectory ++ "/" ++ projFileName
  serializeProjectFile absoluteProjFilePath proj

  -- TODO: These should REALLY be part of project serialization
  let programFilePath = absoluteProjDirectory ++ "/Program.fs"
  writeFile programFilePath $ unlines
    [ "[<EntryPoint>]"
    , "let main argv ="
    , "    printfn \"%A\" argv"
    , "    0"
    ]

  let configFilePath = absoluteProjDirectory ++ "/App.config"
  writeFile configFilePath
    $ ppTopElement
    $ unode "configuration"
    $ unode "startup"
    $ unode "supportedRuntime"
    [ attr "version" "v4.0"
    , attr "sku" ".NETFramework,Version=v4.5"
    ]

  sln <- addProject localProjFilePath proj $ createDefaultSolution $ name info
  let slnFilePath = solutionDirectory ++ "/" ++ name info ++ ".sln"
  serializeSolutionFile slnFilePath sln

attr name value = Attr (unqual name) value -- TODO: Remove when project files are moved
