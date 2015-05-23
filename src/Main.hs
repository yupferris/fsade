import System.Environment
import System.Directory
import System.IO
import Paths_fsade (version)
import Data.Version (showVersion)

main = do
  stringArgs <- getArgs
  let operation = parseArgs stringArgs
  perform operation

data NewProjectInfo = NewProjectInfo {
  name :: String
  } deriving (Show)

data Operation =
  None
  | NewProject NewProjectInfo
  | Invalid
    
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

data Solution = Solution {
  solutionName :: String,
  vsVersion :: VisualStudioVersion,
  minVsVersion :: VisualStudioVersion,
  projects :: [Project],
  globalSections :: [GlobalSection]
  } deriving (Show)

data VisualStudioVersion = VisualStudioVersion {
  a :: Int,
  b :: Int,
  c :: Int,
  d :: Int
  }

instance Show VisualStudioVersion where
  show vsVersion =
    foldr1 (\x y -> x ++ "." ++ y)
    $ map (\f -> show $ f vsVersion) [a, b, c, d]

visualStudioVersion a b c d =
  VisualStudioVersion { a = a, b = b, c = c, d = d }

data Project = Project {
  projectName :: String
  } deriving (Show)

data GlobalSection =
  SolutionConfigurationPlatforms PrePostSolution [String] -- TODO: This is crap :)
  deriving (Show)

data PrePostSolution = PreSolution | PostSolution

instance Show PrePostSolution where
  show PreSolution = "preSolution"
  show PostSolution = "postSolution"

newProject info = do
  let solutionDirectory = name info
  -- createDirectory solutionDirectory

  let sln = Solution {
        solutionName = name info,
        -- These version numbers are fairly arbitrary; stole them
        -- from another solution file. They can probably be safely
        -- adjusted to some degree :)
        vsVersion = visualStudioVersion 12 0 30723 0,
        minVsVersion = visualStudioVersion 10 0 40219 1,
        projects = [],
        globalSections =
          [
            SolutionConfigurationPlatforms
            PreSolution
            [
              "Debug|Any CPU = Debug|Any CPU",
              "Release|Any CPU = Release|Any CPU"
            ]
          ]
        }
  let slnFilePath = solutionDirectory ++ "/" ++ name info ++ ".sln"
  serializeSolution sln slnFilePath

serializeSolution solution filePath =
  withFile filePath WriteMode (serializeSolutionFile solution)

serializeSolutionFile solution handle = do
  let lineCollections =
        concatMap (\f -> f solution)
        [
          (\_ -> serializeSolutionHeader),
          serializeSolutionVersions,
          serializeSolutionGlobalSections
        ]
  let lines = unlines lineCollections
  hPutStr handle lines

serializeSolutionHeader =
  [
    "",
    "Microsoft Visual Studio Solution File, Format Version 12.00",
    ("# fsade " ++ showVersion version)
  ]

serializeSolutionVersions solution =
  [
    "VisualStudioVersion = " ++ show (vsVersion solution),
    "MinimumVisualStudioVersion = " ++ show (minVsVersion solution)
  ]

serializeSolutionGlobalSections solution =
  ["Global"] ++
  (scope $ concatMap serializeSolutionGlobalSection (globalSections solution)) ++
  ["EndGlobal"]

scope = map $ (++) "\t"

serializeSolutionGlobalSection (SolutionConfigurationPlatforms prePost platforms) =
  ["GlobalSection(SolutionConfigurationPlatforms) = " ++ show prePost] ++
  (scope $ serializeSolutionGlobalSectionPlatforms platforms) ++
  ["EndGlobalSection"]

serializeSolutionGlobalSectionPlatforms platforms = platforms

serializeProject _ = do return ()
