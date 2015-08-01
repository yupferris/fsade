module Solution
       ( createDefaultSolution
       , addProject
       , serializeSolutionFile
       ) where

import Data.Char
import Data.Version (showVersion)
import Data.UUID
import Data.UUID.V4
import Paths_fsade (version)
import Project

data Solution = Solution { solutionName :: String
                         , vsVersion :: VisualStudioVersion
                         , minVsVersion :: VisualStudioVersion
                         , projects :: [(UUID, String, Project)]
                         , globalSections :: [GlobalSection]
                         } deriving (Show)

data VisualStudioVersion = VisualStudioVersion { a :: Int
                                               , b :: Int
                                               , c :: Int
                                               , d :: Int
                                               }

instance Show VisualStudioVersion where
  show vsVersion =
    foldl1 (\x y -> x ++ "." ++ y)
    $ map (\f -> show $ f vsVersion) [a, b, c, d]

visualStudioVersion a b c d =
  VisualStudioVersion { a = a
                      , b = b
                      , c = c
                      , d = d
                      }

-- TODO: This type isn't very well thought-out
data GlobalSection = SolutionConfigurationPlatforms PrePostSolution [String]
                   | ProjectConfigurationPlatforms PrePostSolution [(UUID, [String])]
                   | SolutionProperties PrePostSolution Bool
                   deriving (Show)

data PrePostSolution = PreSolution
                     | PostSolution

instance Show PrePostSolution where
  show PreSolution = "preSolution"
  show PostSolution = "postSolution"

createDefaultSolution name =
  -- The version numbers here are fairly arbitrary; stole them
  --  from another solution file. They can probably be safely
  --  adjusted to some degree :)
  Solution { solutionName = name
           , vsVersion = visualStudioVersion 12 0 30723 0
           , minVsVersion = visualStudioVersion 10 0 40219 1
           , projects = []
           , globalSections =
             [ SolutionConfigurationPlatforms
               PreSolution
               [
                 "Debug|Any CPU = Debug|Any CPU",
                 "Release|Any CPU = Release|Any CPU"
               ]
             , SolutionProperties PreSolution False
             ]
           }

-- TODO: This won't correctly handle the case where a project is already in the solution
addProject projectFilePath project solution = do
  solutionLocalProjectGuid <- nextRandom
  return solution { projects = (solutionLocalProjectGuid, projectFilePath, project) : projects solution
                  , globalSections = ProjectConfigurationPlatforms PostSolution
                                     [ (guid project,
                                        [ "Debug|Any CPU.ActiveCfg = Debug|Any CPU"
                                        , "Debug|Any CPU.Build.0 = Debug|Any CPU"
                                        , "Release|Any CPU.ActiveCfg = Release|Any CPU"
                                        , "Release|Any CPU.Build.0 = Release|Any CPU"
                                        ]) ]
                                     : globalSections solution}

serializeSolutionFile filePath = writeFile filePath . serializeSolution

serializeSolution solution =
  unlines $ concatMap (\f -> f solution)
  [
    (\_ -> serializeSolutionHeader),
    serializeSolutionVersions,
    serializeSolutionProjects,
    serializeSolutionGlobalSections
  ]

serializeSolutionHeader =
  [
    "",
    "Microsoft Visual Studio Solution File, Format Version 12.00",
    "# fsade " ++ showVersion version
  ]

serializeSolutionVersions solution =
  [
    "VisualStudioVersion = " ++ show (vsVersion solution),
    "MinimumVisualStudioVersion = " ++ show (minVsVersion solution)
  ]

serializeSolutionProjects solution =
  concatMap serializeSolutionProject $ projects solution

serializeSolutionProject (solutionLocalProjectGuid, projectFilePath, project) =
  ["Project(\"{" ++ (serializeGuid solutionLocalProjectGuid) ++ "}\") = "
   ++ "\"" ++ (projectName project) ++ "\", "
   ++ "\"" ++ (map (\c -> if c == '/' then '\\' else c) projectFilePath) ++ "\", "
   ++ "\"{" ++ (serializeGuid $ guid project)  ++ "}\""] ++
  ["EndProject"]

serializeGuid = map toUpper . toString

serializeSolutionGlobalSections solution =
  ["Global"] ++
  (scope $ concatMap serializeSolutionGlobalSection (globalSections solution)) ++
  ["EndGlobal"]

scope = map $ (++) "\t"

serializeSolutionGlobalSection (SolutionConfigurationPlatforms prePost platforms) =
  serializeSolutionGlobalSectionContents "SolutionConfigurationPlatforms" prePost platforms

serializeSolutionGlobalSection (ProjectConfigurationPlatforms prePost projectConfigurationPlatforms) =
  serializeSolutionGlobalSectionContents "ProjectConfigurationPlatforms" prePost
  $ concatMap (\(guid, strings) ->
                map ((++) $ "{" ++ (map toUpper $ toString guid) ++ "}.") strings) projectConfigurationPlatforms

serializeSolutionGlobalSection (SolutionProperties prePost hideSolutionNode) =
  serializeSolutionGlobalSectionContents "SolutionProperties" prePost
  ["HideSolutionNode = " ++ if hideSolutionNode then "TRUE" else "FALSE"]

serializeSolutionGlobalSectionContents name prePost contents =
  ["GlobalSection(" ++ name ++ ") = " ++ show prePost] ++
  (scope $ contents) ++
  ["EndGlobalSection"]
