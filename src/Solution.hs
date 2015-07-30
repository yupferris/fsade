module Solution
       ( createDefaultSolution
       , serializeSolutionFile
       ) where

import Paths_fsade (version)
import Data.Version (showVersion)

data Solution = Solution { solutionName :: String
                         , vsVersion :: VisualStudioVersion
                         , minVsVersion :: VisualStudioVersion
                         , projects :: [Project]
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

data Project = Project { projectName :: String
                       } deriving (Show)

data GlobalSection =
  SolutionConfigurationPlatforms PrePostSolution [String] -- TODO: This is crap :)
  deriving (Show)

data PrePostSolution = PreSolution
                     | PostSolution

instance Show PrePostSolution where
  show PreSolution = "preSolution"
  show PostSolution = "postSolution"

createDefaultSolution name =
  -- The version numbers here are fairly arbitrary; stole them
  -- from another solution file. They can probably be safely
  -- adjusted to some degree :)
  Solution { solutionName = name
           , vsVersion = visualStudioVersion 12 0 30723 0
           , minVsVersion = visualStudioVersion 10 0 40219 1
           , projects = []
           , globalSections =
             [
               SolutionConfigurationPlatforms
               PreSolution
               [
                 "Debug|Any CPU = Debug|Any CPU",
                 "Release|Any CPU = Release|Any CPU"
               ]
             ]
           }

serializeSolutionFile filePath = writeFile filePath . serializeSolution

serializeSolution solution =
  unlines $ concatMap (\f -> f solution)
  [
    (\_ -> serializeSolutionHeader),
    serializeSolutionVersions,
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
