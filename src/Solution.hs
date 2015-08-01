module Solution
       ( createDefaultSolution
       , serializeSolutionFile
       ) where

import Data.Version (showVersion)
import Paths_fsade (version)
import Project

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

data GlobalSection = SolutionConfigurationPlatforms PrePostSolution [String] -- TODO: This is crap :)
                   | SolutionProperties PrePostSolution Bool -- TODO: This is also crap :P
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
  serializeSolutionGlobalSectionContents "SolutionConfigurationPlatforms" prePost
  $ serializeSolutionGlobalSectionPlatforms platforms

serializeSolutionGlobalSection (SolutionProperties prePost hideSolutionNode) =
  serializeSolutionGlobalSectionContents "SolutionProperties" prePost
  ["HideSolutionNode = " ++ if hideSolutionNode then "TRUE" else "FALSE"]

serializeSolutionGlobalSectionContents name prePost contents =
  ["GlobalSection(" ++ name ++ ") = " ++ show prePost] ++
  (scope $ contents) ++
  ["EndGlobalSection"]

serializeSolutionGlobalSectionPlatforms platforms = platforms
